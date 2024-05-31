use crate::FutureEx;
use alloc::boxed::Box;
use core::{any::Any, future::Future};
use futures_lite::future;

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

pub trait Behavior: Future<Output = bool> {
    fn as_any(&self) -> &dyn Any
    where
        Self: 'static;
    fn as_box_any(self: Box<Self>) -> Box<dyn Any>
    where
        Self: 'static;

    fn not(self) -> impl Future<Output = bool>
    where
        Self: Sized,
    {
        async { !self.await }
    }
}

impl<F: Future<Output = bool>> Behavior for F {
    fn as_any(&self) -> &dyn Any
    where
        Self: 'static,
    {
        self
    }
    fn as_box_any(self: Box<Self>) -> Box<dyn Any>
    where
        Self: 'static,
    {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::future::ready;
    use futures_lite::future::block_on;

    #[test]
    fn any_macro() {
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
