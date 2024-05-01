#![no_std]
#![allow(clippy::async_yields_async)]

extern crate alloc;
use alloc::boxed::Box;

use core::any::Any;
use core::fmt::Debug;
use core::future::{pending, Future};
use core::pin::Pin;
use core::task::Poll;
use futures_lite::FutureExt;

pub use tracing::instrument;

// macros

#[macro_export]
macro_rules! select {
    ($head:expr $(,)?) => { $head };
    ($head:expr, $($tail:tt)*) => {
        futures_lite::future::or($head, select!($($tail)*))
    };
}

#[macro_export]
macro_rules! join {
    ($head:expr $(,)?) => { $head };
    ($head:expr, $($tail:tt)*) => {
        futures_lite::future::zip($head, join!($($tail)*))
    };
}

#[macro_export]
macro_rules! any {
    ($($args:tt)*) => {
        recursive_try_zip!(true, $($args)*).map(|x|x.is_ok()).not()
    };
}

#[macro_export]
macro_rules! all {
    ($($args:tt)*) => {
        recursive_try_zip!(false, $($args)*).map(|x|x.is_ok())
    };
}

#[macro_export]
macro_rules! recursive_try_zip {
    ($inv:expr, $head:expr $(,)?) => { async {($head.await ^ $inv).then_some(()).ok_or(()) } };
    ($inv:expr, $head:expr, $($tail:tt)*) => {
        futures_lite::future::try_zip(recursive_try_zip!($inv, $head), recursive_try_zip!($inv, $($tail)*))
    };
}

#[macro_export]
macro_rules! repeat_until {
    ($f:expr, $until:expr $(,)?) => {
        async {
            while $f.await ^ $until {}
            true
        }
    };
    ($f:expr, $until:expr, $n:expr $(,)?) => {
        async {
            for _ in 0..$n {
                if $f.await ^ !$until {
                    return true;
                }
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

impl<T: Any> Debug for dyn FutureEx<Output = T> {
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

impl<F: Future<Output = State> + Any> From<F> for State {
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

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::rc::Rc;
    use core::cell::Cell;
    use core::future::ready;
    use futures_lite::future::block_on;

    fn test_init() {
        let _ = tracing_subscriber::fmt()
            .with_span_events(tracing_subscriber::fmt::format::FmtSpan::NEW)
            .with_target(false)
            .try_init();
    }

    #[instrument(skip(wm))]
    pub async fn state_a(wm: impl WorldModel) -> State {
        state_b(wm.clone()).into()
    }

    #[instrument(skip(wm))]
    pub async fn state_b(wm: impl WorldModel) -> State {
        state_c(wm.clone()).into()
    }

    #[instrument(skip(wm))]
    pub async fn state_c(wm: impl WorldModel) -> State {
        select!(
            transition(ready(false), None, None),
            transition(ready(true), Some(State::Success), None),
            transition(ready(true), Some(State::Success), None),
            transition(ready(true), Some(State::Success), None),
            transition_if!(ready(true).not(), State::Success),
            transition_if!(ready(true).not(), state_c(wm.clone()).into()),
        )
        .await
    }

    // choose a tune to hum
    #[instrument(skip(_wm), ret)]
    pub async fn choose_tune(_wm: impl WorldModel) -> bool {
        select!(ready(true), ready(true), ready(true)).await
        // (X(hum_a_tune(wm.clone(), 2)) | X(hum_a_tune(wm, 3)) ).0.await
        // (X(hum_a_tune(wm.clone(), 2)) | X(hum_a_tune(wm.clone(), 3)) | X(hum_a_tune(wm, 3))) .0 .await
        //((X(hum_a_tune(wm.clone(), 2)) | X(hum_a_tune(wm, 3))) | X(hum_a_tune(wm, 4))).0.await
    }

    #[instrument(skip(_wm), ret)]
    pub async fn hum_a_tune(_wm: impl WorldModel, id: u8) -> bool {
        false
    }

    #[derive(Debug, Default)]
    pub struct InnerWorldModel {
        pub enemy_near: Cell<bool>,
        pub moved_to_enemy: Cell<bool>,
        pub attacked: Cell<bool>,
        pub is_on_grass: Cell<bool>,
        pub sat_down: Cell<bool>,
        pub waited: Cell<bool>,
        pub hummed: Cell<u8>,
    }

    pub trait WorldModel: Any + Clone + Debug {}

    #[derive(Debug, Default, Clone)]
    pub struct SharedWorldModel(pub Rc<InnerWorldModel>);
    impl WorldModel for SharedWorldModel {}

    impl InnerWorldModel {
        #[instrument(skip(self), ret)]
        pub async fn root(&self) -> bool {
            self.handle_enemy().await
                || self.chill_on_grass().await
                || self.choose_tune().await
                || self.choose_tune2().await
        }

        // choose a tune to hum
        pub async fn choose_tune(&self) -> bool {
            any!(self.hum_a_tune(2), self.hum_a_tune(1)).await
        }

        pub async fn choose_tune2(&self) -> bool {
            //try_zip!(ready(true), ready(true)).await
            all!(ready(true), ready(true)).await
        }

        #[instrument(skip(self), ret)]
        pub async fn hum_a_tune(&self, id: u8) -> bool {
            if self.hummed.get() != 0 {
                return false;
            }
            self.hummed.set(id);
            true
        }

        // handle enemy
        #[instrument(skip(self), ret)]
        pub async fn handle_enemy(&self) -> bool {
            self.is_enemy_near().await & self.move_to_enemy().await & self.attack_enemy().await
        }

        #[instrument(skip(self), ret)]
        pub async fn is_enemy_near(&self) -> bool {
            self.enemy_near.get()
        }

        #[instrument(skip(self), ret)]
        pub async fn move_to_enemy(&self) -> bool {
            if self.moved_to_enemy.get() {
                return false;
            }
            self.moved_to_enemy.set(true);
            true
        }

        #[instrument(skip(self), ret)]
        pub async fn attack_enemy(&self) -> bool {
            if self.attacked.get() {
                return false;
            }
            self.attacked.set(true);
            true
        }

        // chill on grass
        #[instrument(skip(self), ret)]
        pub async fn chill_on_grass(&self) -> bool {
            self.standing_on_grass().await
                & self.sit_down().await
                & self.delay().await
                & self.stand_up().await
        }

        #[instrument(skip(self), ret)]
        pub async fn standing_on_grass(&self) -> bool {
            !self.sat_down.get() & self.is_on_grass.get()
        }

        #[instrument(skip(self), ret)]
        pub async fn sit_down(&self) -> bool {
            if self.sat_down.get() {
                return false;
            }
            self.sat_down.set(true);
            true
        }

        #[instrument(skip(self), ret)]
        pub async fn delay(&self) -> bool {
            if self.waited.get() {
                return false;
            }
            self.waited.set(true);
            true
        }

        #[instrument(skip(self), ret)]
        pub async fn stand_up(&self) -> bool {
            if !self.sat_down.get() {
                return false;
            }
            self.sat_down.set(true);
            true
        }
    }

    #[test]
    fn state_machine_test() {
        test_init();
        let wm = SharedWorldModel::default();
        let root = State::from(state_a(wm));
        let ret = futures_lite::future::block_on(root);
        tracing::debug!(ret);
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

    #[test]
    fn behaviour_tree_test() {
        test_init();

        let ctx = InnerWorldModel {
            enemy_near: false.into(),
            moved_to_enemy: false.into(),
            attacked: false.into(),

            is_on_grass: false.into(),
            sat_down: false.into(),
            waited: false.into(),

            hummed: 0.into(),
        };

        assert!(block_on(ctx.root()));
        tracing::debug!(?ctx);
    }
}
