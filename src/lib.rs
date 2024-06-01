#![no_std]
#![allow(clippy::async_yields_async)]

extern crate alloc;

pub use core::future::{ready, Future};
pub use futures_lite::{future, FutureExt};
pub use tracing::instrument;

pub use behavior::*;
pub use state::*;

use core::pin::Pin;
use core::task::{Context, Poll};
use pin_project::pin_project;

#[macro_export]
macro_rules! join {
    ($head:expr) => { $head };
    ($head:expr, $($tail:expr),+ $(,)?) => {
        future::zip($head, join!($($tail),+))
    };
}

#[macro_export]
macro_rules! select {
    ($head:expr) => { $head };
    ($head:expr, $($tail:expr),+ $(,)?) => {
        future::or($head, select!($($tail),+))
    };
}

#[macro_export]
macro_rules! select_state {
    ($head:expr $(,)?) => { $head };
    ($head:expr, $($tail:expr),+ $(,)?) => {
        select_state($head, select_state!($($tail),+))
    };
}

pub fn select_state<A: Future, B: Future>(
    a: A,
    b: B,
) -> future::Or<Selection<A, B>, Selection<A, B>> {
    future::or(Selection::A(a), Selection::B(b))
}

#[pin_project(project = SelectionProj)]
pub enum Selection<A, B> {
    A(#[pin] A),
    B(#[pin] B),
}

impl<A: Future, B: Future> Future for Selection<A, B> {
    type Output = Either<A::Output, B::Output>;
    fn poll(self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.project() {
            SelectionProj::A(a) => {
                if let Poll::Ready(x) = a.poll(ctx) {
                    return Poll::Ready(Either::A(x));
                }
            }
            SelectionProj::B(b) => {
                if let Poll::Ready(x) = b.poll(ctx) {
                    return Poll::Ready(Either::B(x));
                }
            }
        };
        Poll::Pending
    }
}

#[pin_project(project = EitherProj)]
pub enum Either<A, B> {
    A(#[pin] A),
    B(#[pin] B),
}

impl<O, A: Future<Output = O>, B: Future<Output = O>> Future for Either<A, B> {
    type Output = O;
    fn poll(self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<O> {
        {
            match self.project() {
                EitherProj::A(a) => a.poll(ctx),
                EitherProj::B(b) => b.poll(ctx),
            }
        }
    }
}

// Future trait extensions
pub trait FutureMap: Future {
    /*
    fn type_name(&self) -> &'static str {
        core::any::type_name::<Self>()
    }
    */

    fn map<O>(self, f: impl FnOnce(<Self as Future>::Output) -> O) -> impl Future<Output = O>
    where
        Self: Sized,
    {
        async { f(self.await) }
    }

    fn force<O>(self, o: O) -> impl Future<Output = O>
    where
        Self: Sized,
    {
        self.map(move |_| o)
    }
}

impl<F: Future> FutureMap for F {}

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
mod state;

#[instrument]
pub async fn state0() -> bool {
    state1().await
}

#[instrument]
pub async fn state1() -> bool {
    state2().await
}

#[instrument]
pub async fn state2() -> bool {
    state3().await
}

#[instrument]
pub async fn state3() -> bool {
    true
}

#[instrument]
pub async fn state_sel() -> bool {
    select_state!(state0().then(state1()), state2().then(state3()),)
        .await
        .await
}

#[instrument]
pub async fn state_sel2() -> bool {
    select_state!(state0().then(state1()), state2().then(state3()),)
        .await
        .await
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_lite::future::block_on;

    #[test]
    fn simple_test() {
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_span_events(tracing_subscriber::fmt::format::FmtSpan::NEW)
            .with_target(false)
            .try_init();

        assert!(block_on(state_sel()));
        tracing::debug!("i");
    }
}
