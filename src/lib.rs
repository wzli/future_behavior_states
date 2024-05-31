#![no_std]
#![allow(clippy::async_yields_async)]
extern crate alloc;

use core::fmt::Debug;
use core::future::Future;
use core::any::Any;
use alloc::boxed::Box;

pub use futures_lite::{future, FutureExt};
pub use tracing::instrument;
pub use behavior::Behavior;

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

}

impl<F: Future> FutureEx for F {}

/*
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
*/

mod behavior;
//mod experiment;
//mod state;
//mod static_state;
mod new;

#[cfg(test)]
mod tests {
    use super::*;

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
