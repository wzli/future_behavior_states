use futures_lite::{future, FutureExt};
use std::any::{Any, TypeId};
use std::cell::Cell;
use std::fmt::Debug;
use std::future::{pending, ready, Future};
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll};
use tracing::instrument;

use std::cell::OnceCell;

// macros

macro_rules! select {
    ($num:expr, $($tail:tt)*) => {
        future::race($num, select!($($tail)*))
    };
    ($num:expr) => { $num };
}
pub(crate) use select;

macro_rules! join {
    ($num:expr, $($tail:tt)*) => {
        future::zip($num, join!($($tail)*))
    };
    ($num:expr) => { $num };
}
pub(crate) use join;

macro_rules! all {
    ($num:expr, $($tail:tt)*) => {
        future::try_zip(to_result($num), all!($($tail)*).map(to_result)).map(to_bool)
    };
    ($num:expr) => { $num };
}
pub(crate) use all;

macro_rules! any {
    ($num:expr, $($tail:tt)*) => {
        future::try_zip(to_result(not($num)), any!($($tail)*).map(to_result)).map(to_bool).map(not)
    };
    ($num:expr) => { $num };
}
pub(crate) use any;

// traits
pub trait FutureState: Future<Output = State> + Any {}
impl<F: Future<Output = State> + Any> FutureState for F {}

pub trait Map: Sized {
    fn map<O, F: FnOnce(Self) -> O>(self, f: F) -> O {
        f(self)
    }
}
impl<T, F: Future<Output = T>> Map for F {}

struct State(Pin<Box<dyn FutureState>>);

// trait implementations

impl PartialEq for dyn FutureState {
    fn eq(&self, other: &Self) -> bool {
        self.type_id() == other.type_id()
    }
}

impl<F: FutureState> From<F> for State {
    fn from(f: F) -> State {
        State(Box::pin(f))
    }
}

impl Future for State {
    type Output = bool;
    fn poll(mut self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        if let Poll::Ready(next_state) = self.0.poll(ctx) {
            if self.0 == State::from(success()).0 {
                Poll::Ready(true)
            } else if self.0 == State::from(failure()).0 {
                Poll::Ready(false)
            } else {
                *self = next_state;
                ctx.waker().wake_by_ref();
                Poll::Pending
            }
        } else {
            Poll::Pending
        }
    }
}

// helpers

#[instrument]
pub async fn success() -> State {
    success().into()
}

#[instrument]
pub async fn failure() -> State {
    failure().into()
}

#[instrument(skip(f))]
pub async fn not(f: impl Future<Output = bool>) -> bool {
    !f.await
}

// TODO: this needs to be macro to avoid requirement for copy
/*
#[instrument(skip(f))]
pub async fn RunUntil(f: impl Future<Output = bool>, terminate_on: bool, result: bool) -> bool {
    while f.await != terminate_on {};
    result
}
*/

async fn to_bool<T, E>(f: impl Future<Output = Result<T, E>>) -> bool {
    f.await.is_ok()
}

async fn to_result(f: impl Future<Output = bool>) -> Result<(), ()> {
    if f.await {
        Ok(())
    } else {
        Err(())
    }
}

#[instrument(skip(f, state))]
pub async fn transition_on_result(
    f: impl Future<Output = bool>,
    state: Option<State>,
    on_success: bool,
    on_failure: bool,
) -> State {
    if let Some(state) = state {
        let result = f.await;
        if (result && on_success) || (!result && on_failure) {
            return state;
        }
    }
    pending().await
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
        transition_on_result(ready(true), Some(success().into()), true, true),
        transition_on_result(ready(false), None, true, true)
    )
    .await
}

// choose a tune to hum
#[instrument(skip(wm), ret)]
pub async fn choose_tune(wm: impl WorldModel) -> bool {
    select!(ready(true), ready(true), ready(true)).await
    // (X(hum_a_tune(wm.clone(), 2)) | X(hum_a_tune(wm, 3)) ).0.await
    // (X(hum_a_tune(wm.clone(), 2)) | X(hum_a_tune(wm.clone(), 3)) | X(hum_a_tune(wm, 3))) .0 .await
    //((X(hum_a_tune(wm.clone(), 2)) | X(hum_a_tune(wm, 3))) | X(hum_a_tune(wm, 4))).0.await
}

#[instrument(skip(wm), ret)]
pub async fn hum_a_tune(wm: impl WorldModel, id: u8) -> bool {
    false
}

#[derive(Debug, Default)]
struct InnerWorldModel {
    enemy_near: Cell<bool>,
    moved_to_enemy: Cell<bool>,
    attacked: Cell<bool>,
    is_on_grass: Cell<bool>,
    sat_down: Cell<bool>,
    waited: Cell<bool>,
    hummed: Cell<u8>,
}

trait WorldModel: Any + Clone + Debug {}

#[derive(Debug, Default, Clone)]
struct SharedWorldModel(pub Rc<InnerWorldModel>);
impl WorldModel for SharedWorldModel {}

impl InnerWorldModel {
    #[instrument(skip(self), ret)]
    pub async fn root(&self) -> bool {
        self.handle_enemy().await || self.chill_on_grass().await || self.choose_tune().await
    }

    // choose a tune to hum
    pub async fn choose_tune(&self) -> bool {
        future::or(self.hum_a_tune(2), self.hum_a_tune(1)).await
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

#[cfg(test)]
mod tests {
    use super::*;

    fn test_init() {
        let _ = tracing_subscriber::fmt()
            .with_span_events(tracing_subscriber::fmt::format::FmtSpan::NEW)
            .with_target(false)
            .try_init();
    }

    #[test]
    fn state_equality_checks() {
        let a = State::from(success());
        let b = State::from(success());
        let c = State::from(failure());
        assert!(a.0 == b.0);
        assert!(b.0 != c.0);
        assert!(a.0 != c.0);
    }

    #[test]
    fn state_machine_test() {
        test_init();
        let wm = SharedWorldModel::default();
        let root = State::from(state_a(wm));
        let ret = futures_lite::future::block_on(root);
        dbg!(ret);
    }

    async fn test_root() -> bool {
        any!(ready(true), ready(false), ready(false)).await
    }

    async fn choose_tune2() -> bool {
        //try_zip!(ready(true), ready(true)).await
        all!(ready(true), ready(true), ready(true)).await
    }

    #[test]
    fn try_zip() {
        test_init();
    }

    #[test]
    fn behaviour_tree_test() {
        test_init();

        let mut ctx = InnerWorldModel {
            enemy_near: false.into(),
            moved_to_enemy: false.into(),
            attacked: false.into(),

            is_on_grass: false.into(),
            sat_down: false.into(),
            waited: false.into(),

            hummed: 0.into(),
        };

        assert!(futures_lite::future::block_on(ctx.root()));
        dbg!(ctx);
    }
}
