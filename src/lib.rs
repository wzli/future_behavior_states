#![no_std]
#![allow(clippy::async_yields_async)]

extern crate alloc;

pub use core::future::{ready, Future};
pub use futures_lite::{future, FutureExt};
pub use tracing::instrument;

pub use behavior::*;
pub use state::*;

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

#[cfg(test)]
mod tests {
    //use super::*;
    //use futures_lite::future::block_on;
}
