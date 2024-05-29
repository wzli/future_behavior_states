#![no_std]
#![allow(clippy::async_yields_async)]
extern crate alloc;
use alloc::boxed::Box;

use core::fmt::Debug;
use core::future::{pending, Future};
use core::pin::Pin;
use core::task::Poll;

pub use futures_lite::{future, FutureExt};
pub use tracing::instrument;

// macros

#[macro_export]
macro_rules! select {
    ($head:expr) => { $head };
    ($head:expr, $($tail:expr),+ $(,)?) => {
        future::or($head, select!($($tail),+))
    };
}

#[macro_export]
macro_rules! join {
    ($head:expr) => { $head };
    ($head:expr, $($tail:expr),+ $(,)?) => {
        future::zip($head, join!($($tail),+))
    };
}

#[macro_export]
macro_rules! any {
    ($($args:expr),+ $(,)?) => {
        recursive_try_zip!(true, $($args),+).map(|x|x.is_ok()).not()
    };
}

#[macro_export]
macro_rules! all {
    ($($args:expr),+ $(,)?) => {
        recursive_try_zip!(false, $($args),+).map(|x|x.is_ok())
    };
}

#[macro_export]
macro_rules! recursive_try_zip {
    ($inv:expr, $head:expr) => { async {($head.await ^ $inv).then_some(()).ok_or(()) } };
    ($inv:expr, $head:expr, $($tail:expr),+  $(,)?) => {
        future::try_zip(recursive_try_zip!($inv, $head), recursive_try_zip!($inv, $($tail),+))
    };
}

#[macro_export]
macro_rules! repeat_until {
    ($f:expr, $until:expr $(,)?) => {
        repeat_until!($f, $until, usize::MAX)
    };
    ($f:expr, $until:expr, $n:expr $(,)?) => {
        async {
            for _ in 0..$n {
                if $f.await ^ !$until {
                    return true;
                }
                future::yield_now().await
            }
            false
        }
    };
}

#[macro_export]
macro_rules! transition_if {
    ($f:expr, $s:expr $(,)?) => {
        transition(repeat_until!($f, true), Some($s), None)
    };
}

// Future trait extensions
pub trait FutureEx: Future {
    fn type_name(&self) -> &'static str {
        core::any::type_name::<Self>()
    }

    fn map<O>(self, f: impl FnOnce(<Self as Future>::Output) -> O) -> impl Future<Output = O>
    where
        Self: Sized,
    {
        async { f(self.await) }
    }

    fn not(self) -> impl Future<Output = bool>
    where
        Self: Future<Output = bool> + Sized,
    {
        async { !self.await }
    }
}

impl<T, F: Future<Output = T>> FutureEx for F {}

impl<T> Debug for dyn FutureEx<Output = T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let s = self.type_name();
        for token in s
            .strip_suffix("::{{closure}}")
            .unwrap_or(s)
            .split('<')
            .flat_map(|s| s.split('>'))
            .step_by(2)
        {
            f.write_str(token)?
        }
        Ok(())
    }
}

// State implementation
#[derive(Debug)]
pub enum State {
    Success,
    Failure,
    Running(Pin<Box<dyn FutureEx<Output = State>>>),
}

impl<F: Future<Output = State> + 'static> From<F> for State {
    fn from(f: F) -> State {
        State::Running(Box::pin(f))
    }
}

impl Future for State {
    type Output = bool;
    fn poll(mut self: Pin<&mut Self>, ctx: &mut core::task::Context<'_>) -> Poll<Self::Output> {
        match &mut *self {
            State::Success => Poll::Ready(true),
            State::Failure => Poll::Ready(false),
            State::Running(state) => {
                if let Poll::Ready(next_state) = state.poll(ctx) {
                    *self = next_state;
                    ctx.waker().wake_by_ref();
                }
                Poll::Pending
            }
        }
    }
}

#[instrument(skip(f), ret)]
pub async fn transition(
    f: impl Future<Output = bool>,
    success_state: Option<State>,
    failure_state: Option<State>,
) -> State {
    if f.await {
        if let Some(state) = success_state {
            return state;
        }
    } else if let Some(state) = failure_state {
        return state;
    }
    pending().await
}

mod behavior;
mod state;
mod static_state;
mod experiment;

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::rc::Rc;
    use async_broadcast::{broadcast, Receiver, Sender};
    use core::cell::Cell;
    use core::future::ready;
    use futures_lite::future::block_on;

    pub fn test_init() {
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_span_events(tracing_subscriber::fmt::format::FmtSpan::NEW)
            .with_target(false)
            .try_init();
    }

    #[test]
    fn any_macro() {
        test_init();
        async fn root(a: bool, b: bool) -> bool {
            any!(ready(a), ready(b)).await
        }
        assert!(!block_on(root(false, false)));
        assert!(block_on(root(false, true)));
        assert!(block_on(root(true, false)));
        assert!(block_on(root(true, true)));
    }

    #[test]
    fn all_macro() {
        test_init();
        async fn root(a: bool, b: bool) -> bool {
            all!(ready(a), ready(b),).await
        }
        assert!(!block_on(root(false, false)));
        assert!(!block_on(root(false, true)));
        assert!(!block_on(root(true, false)));
        assert!(block_on(root(true, true)));
    }

    #[test]
    fn run_until_macro() {
        test_init();

        #[instrument]
        async fn count_down(x: &mut u8) -> bool {
            if *x == 0 {
                true
            } else {
                *x -= 1;
                false
            }
        }

        let mut x = 3;
        assert!(block_on(repeat_until!(count_down(&mut x), true)));
        x = 3;
        assert!(block_on(repeat_until!(count_down(&mut x), true, 4)));
        x = 3;
        assert!(!block_on(repeat_until!(count_down(&mut x), true, 3)));
        assert_eq!(x, 0);
    }
}
