use crate::*;

#[macro_export]
macro_rules! parallel_any {
    ($($args:expr),+ $(,)?) => { recursive_try_zip!(true, $($args),+).map(|x|x.is_ok()).not() };
}

#[macro_export]
macro_rules! parallel_all {
    ($($args:expr),+ $(,)?) => { recursive_try_zip!(false, $($args),+).map(|x|x.is_ok()) };
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
        async { $x.await && future::or(
                repeat_until(usize::MAX, Some(false), || $x).not(),
                reactive_sequence!($($rest),+),
        ).await }
    };
}

#[macro_export]
macro_rules! reactive_selector {
    ($x:expr $(,)?) => { $x };
    ($x:expr, $($rest:expr),+ $(,)?) => {
        async { $x.await || future::or(
                repeat_until(usize::MAX, Some(true), || $x),
                reactive_selector!($($rest),+),
        ).await }
    };
}

pub async fn repeat_until<O: PartialEq, F: Future<Output = O>>(
    attempts: usize,
    value: Option<O>,
    f: impl Fn() -> F,
) -> bool {
    for _ in 0..attempts {
        let res = f().await;
        if let Some(val) = &value {
            if res == *val {
                return true;
            }
        }
        future::yield_now().await
    }
    false
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
        assert!(block_on(repeat_until(100, Some(true), || count_down(&x))));
        x.set(3);
        assert!(block_on(repeat_until(4, Some(true), || count_down(&x))));
        x.set(3);
        assert!(!block_on(repeat_until(3, Some(true), || count_down(&x))));
        assert_eq!(x.get(), 0);
    }
}
