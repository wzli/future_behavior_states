#![no_std]
#![allow(clippy::async_yields_async)]

extern crate alloc;

pub use core::future::{ready, Future};
pub use futures_lite::{future, FutureExt};
pub use tracing::instrument;

pub use behavior::*;
pub use state::*;

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

// Future trait extensions
pub trait FutureEx: Future + Sized {
    fn map<O>(self, f: impl FnOnce(<Self as Future>::Output) -> O) -> impl Future<Output = O> {
        async { f(self.await) }
    }

    fn force<O>(self, o: O) -> impl Future<Output = O> {
        self.map(move |_| o)
    }

    fn not(self) -> impl Future<Output = bool>
    where
        Self: Future<Output = bool>,
    {
        async { !self.await }
    }
}

impl<F: Future> FutureEx for F {}

mod behavior;
mod state;

#[instrument]
async fn state_a(b: bool) -> State {
    if b {
        success()
    } else {
        failure()
    }
}

#[instrument]
async fn state_d() -> State {
    state_a(true).into()
}

#[instrument]
async fn state_e() -> State {
    state_d().into()
}

#[instrument]
async fn state_b() -> State {
    state_c().into()
}

#[instrument]
fn state_c() -> impl Future<Output = State> {
    async { state_b().into() }
}

#[cfg(test)]
mod tests {
    use super::*;
    use future::block_on;

    #[test]
    fn simple_test() {
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_span_events(
                tracing_subscriber::fmt::format::FmtSpan::NEW
                    | tracing_subscriber::fmt::format::FmtSpan::CLOSE,
            )
            .with_target(false)
            .try_init();

        assert!(block_on(state_a(true).eval()));
        assert!(block_on(state_e().eval()));
    }

    /*
    #[test]
    fn comp_test() {
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_span_events(
                tracing_subscriber::fmt::format::FmtSpan::NEW
                    | tracing_subscriber::fmt::format::FmtSpan::CLOSE,
            )
            .with_target(false)
            .try_init();
        let x = Xstate::from(xsuccess());
        let y = Xstate::from(xfailure());
        let xx : &dyn State= &xsuccess();
        let yy : &dyn State= &xfailure();
        assert_ne!(xsuccess().type_id(), xfailure().type_id());
        assert_ne!(xx.id(), yy.id());

        assert_eq!((*x.0).id(), xsuccess().type_id());
        assert_ne!((*x.0).id(), xfailure().type_id());
    }

    #[test]
    fn simple_test() {
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_span_events(
                tracing_subscriber::fmt::format::FmtSpan::NEW
                    | tracing_subscriber::fmt::format::FmtSpan::CLOSE,
            )
            .with_target(false)
            .try_init();

        assert!(block_on(state_sel()));
        //assert!(block_on(state_y().into_dyn()));
        //assert!(block_on(async {state_dd().await.await} ));
        //assert!(block_on(state_dd());
        //assert!(block_on(xsuccess().eval()));
        assert!(block_on(state_dd().eval()));
        tracing::debug!("i");
    }
    */
}
