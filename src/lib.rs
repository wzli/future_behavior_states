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
