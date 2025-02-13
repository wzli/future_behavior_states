use crate::*;

#[macro_export]
macro_rules! parallel_any {
    ($($args:expr),+ $(,)?) => {
        recursive_try_zip!(true, $($args),+).map(|x|x.is_ok()).not()
    };
}

#[macro_export]
macro_rules! parallel_all {
    ($($args:expr),+ $(,)?) => {
        recursive_try_zip!(false, $($args),+).map(|x|x.is_ok())
    };
}

#[macro_export]
macro_rules! recursive_try_zip {
    ($inv:expr, $head:expr $(,)?) => { async {($head.await ^ $inv).then_some(()).ok_or(()) } };
    ($inv:expr, $head:expr, $($tail:expr),+  $(,)?) => {
        future::try_zip(recursive_try_zip!($inv, $head), recursive_try_zip!($inv, $($tail),+))
    };
}

#[macro_export]
macro_rules! reactive_sequence {
    ($x:expr $(,)?) => { $x };
    ($x:expr, $($rest:expr),+ $(,)?) => {
        async { $x.await && parallel_all!($x, $($rest),+).await }
    };
}

#[macro_export]
macro_rules! reactive_selector {
    ($x:expr $(,)?) => { $x };
    ($x:expr, $($rest:expr),+ $(,)?) => {
        async { $x.await || parallel_any!($x, $($rest),+).await }
    };
}

pub struct Repeat<const ATTEMPTS: usize = { usize::MAX }>;

impl<const ATTEMPTS: usize> Repeat<ATTEMPTS> {
    pub async fn until<O: PartialEq, F: Future<Output = O>>(o: O, f: impl Fn() -> F) -> bool {
        for _ in 0..ATTEMPTS {
            if f().await == o {
                return true;
            }
            future::yield_now().await
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::cell::Cell;
    use future::block_on;

    #[test]
    fn parallel_any_macro() {
        async fn root(a: bool, b: bool) -> bool {
            parallel_any!(ready(a), ready(b)).await
        }
        assert!(!block_on(root(false, false)));
        assert!(block_on(root(false, true)));
        assert!(block_on(root(true, false)));
        assert!(block_on(root(true, true)));
    }

    #[test]
    fn parallel_all_macro() {
        async fn root(a: bool, b: bool) -> bool {
            parallel_all!(ready(a), ready(b),).await
        }
        assert!(!block_on(root(false, false)));
        assert!(!block_on(root(false, true)));
        assert!(!block_on(root(true, false)));
        assert!(block_on(root(true, true)));
    }

    #[test]
    fn reactive_sequence_macro() {
        async fn root(a: bool, b: bool) -> bool {
            reactive_sequence!(ready(a), ready(b),).await
        }
        assert!(!block_on(root(false, false)));
        assert!(!block_on(root(false, true)));
        assert!(!block_on(root(true, false)));
        assert!(block_on(root(true, true)));
    }

    #[test]
    fn reactive_selector_macro() {
        async fn root(a: bool, b: bool) -> bool {
            reactive_selector!(ready(a), ready(b)).await
        }
        assert!(!block_on(root(false, false)));
        assert!(block_on(root(false, true)));
        assert!(block_on(root(true, false)));
        assert!(block_on(root(true, true)));
    }

    #[test]
    fn run_until_macro() {
        async fn count_down(x: &Cell<u8>) -> bool {
            if x.get() == 0 {
                true
            } else {
                x.set(x.get() - 1);
                false
            }
        }

        let x = Cell::new(3);
        assert!(block_on(<Repeat>::until(true, || count_down(&x))));
        x.set(3);
        assert!(block_on(Repeat::<4>::until(true, || count_down(&x))));
        x.set(3);
        assert!(!block_on(Repeat::<3>::until(true, || count_down(&x))));
        assert_eq!(x.get(), 0);
    }
}
