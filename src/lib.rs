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

    fn not<O: core::ops::Not<Output = O>>(self) -> impl Future<Output = O>
    where
        Self: Future<Output = O>,
    {
        async { !self.await }
    }
}

impl<F: Future> FutureEx for F {}

mod behavior;
mod state;

#[cfg(test)]
mod tests {
    use super::*;
    use future::block_on;

    #[test]
    fn join_macro() {
        let x = block_on(join!(ready(true), ready(false), ready(true), ready(false)));
        assert_eq!(x, (true, (false, (true, false))));
    }

    #[test]
    fn select_macro() {
        let x = block_on(select!(
            future::pending(),
            future::pending(),
            future::pending(),
            ready(true)
        ));
        assert!(x);
    }

    #[test]
    fn map_trait() {
        let x = block_on(ready(5).map(|x| x * 2));
        assert_eq!(x, 10);
    }

    #[test]
    fn force_trait() {
        let x = block_on(ready(5).force(true));
        assert!(x);
    }

    #[test]
    fn not_trait() {
        let x = block_on(ready(false).not());
        assert!(x);
    }
}
