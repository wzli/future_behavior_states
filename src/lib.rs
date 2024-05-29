#![no_std]
#![allow(clippy::async_yields_async)]
extern crate alloc;
use alloc::boxed::Box;

use core::fmt::Debug;
use core::future::{pending, Future};
use core::pin::Pin;
use core::task::Poll;

pub use futures_lite::{future, FutureExt};
pub use tracing::instrument;

use pin_project::pin_project;

struct StateFuture<T>(fn(&T) -> StateFuture<T>);

impl<T> StateFuture<T> {
    async fn fut(&self, t: &T) -> Self {
        self.0(t)
    }

    #[instrument(skip(self, t), ret)]
    async fn evaluate(&self, t: &T) -> bool {
        let mut state = self.fut(t).await;
        loop {
            if state.0 == success {
                return true;
            } else if state.0 == failure {
                return false;
            } else {
                state = state.fut(t).await;
            }
        }
        /*
        loop {
            match state.0 {
                success => return true,
                failure => return false,
                _ => {state = state.fut(t).await; }
            }
        }

        while state.0 != terminal_state {
            state = state.fut(t).await;
        }
        let mut state = self.fut(t).await;
        loop {
            match  state {
                Ok(next_state) => { state = next_state.fut(t).await; },
                Err(result) => return result,
            }
        }
        */
    }
}

#[instrument(skip(t))]
fn success<T>(t: &T) -> StateFuture<T> {
    StateFuture(success)
}

#[instrument(skip(t))]
fn failure<T>(t: &T) -> StateFuture<T> {
    StateFuture(failure)
}

struct Ctx;

impl Ctx {
    #[instrument(skip(self))]
    fn state_fut_0(&self) -> StateFuture<Self> {
        StateFuture(Self::state_fut_1)
    }

    #[instrument(skip(self))]
    fn state_fut_1(&self) -> StateFuture<Self> {
        StateFuture(Self::state_fut_2)
    }

    #[instrument(skip(self))]
    fn state_fut_2(&self) -> StateFuture<Self> {
        StateFuture(success)
        //StateFuture(Self::state_fut_0)
    }
}

async fn new_test_fn() -> bool {
    let ctx = Ctx;
    ctx.state_fut_0().evaluate(&ctx).await

    /*
    for _ in 0..10 {
        s = s.fut(&ctx).await;
        if s.0 == Ctx::state_fut_0 {
            break;
        }
    }
    */
}

pub trait StateFn {
    fn fut(self) -> impl StateFn;
}

trait ST<T> {
    fn call(self) -> T;
}

struct S<T>(fn() -> T);

impl<O, T: ST<O>> ST<O> for S<T> {
    fn call(self) -> O {
        self.0().call()
    }
}

impl<T> StateFn for S<T>
where
    T: StateFn,
{
    fn fut(self) -> impl StateFn {
        self.0()
    }
}

struct X(S<S<S<S<X>>>>);

struct Y<S: ST<Y<S>>>(S);

struct Z(fn() -> Z);

struct W(S<W>);

struct W2(S<S<W2>>);

impl StateFn for Z {
    fn fut(self) -> impl StateFn {
        self.0()
    }
}

impl StateFn for W {
    fn fut(self) -> impl StateFn {
        self.0 .0()
    }
}

impl StateFn for W2 {
    fn fut(self) -> impl StateFn {
        self.0 .0()
    }
}

impl StateFn for K<bool> {
    fn fut(self) -> impl StateFn {
        self
    }
}

#[instrument]
fn state_fn_0() -> impl StateFn {
    //W(S(state_fn_0))
    //WA(S(state_fn_1))
    //S(state_fn_0)
    K(false)
}
/*

#[instrument]
fn state_fn_1() -> impl StateFn {
    //S(state_fn_0)
    //W(S(state_fn_1))
    WB(S(state_fn_0))
}

#[instrument]
async fn state_fn_2() -> impl StateFn {
    state_fn_1
}
*/

fn cycle_fn() {
    //let x = state_fn_0.fut().fut().fut();
    let s = S(state_fn_0);
    let s = s.fut();
    let s = s.fut();
    let s = s.fut();

    let s = S(state_fn_0);
    let s = s.fut();
    let s = s.fut();
    let s = s.fut();
}

/*
struct X;
impl StateFn for X {
    type A = ();
}

fn what() -> impl StateFn {
    X
}
*/

#[pin_project(project = EnumProj)]
pub enum F2<A, B> {
    A(#[pin] A),
    B(#[pin] B),
}

pub struct K<T>(T);

impl<A: Future, B: Future> Future for F2<A, B> {
    type Output = K<F2<A::Output, B::Output>>;
    fn poll(self: Pin<&mut Self>, ctx: &mut core::task::Context<'_>) -> Poll<Self::Output> {
        match self.project() {
            EnumProj::A(a) => {
                if let Poll::Ready(x) = a.poll(ctx) {
                    return Poll::Ready(K(F2::A(x)));
                }
            }
            EnumProj::B(b) => {
                if let Poll::Ready(x) = b.poll(ctx) {
                    return Poll::Ready(K(F2::B(x)));
                }
            }
        };
        Poll::Pending
    }
}

pub trait StateItr {
    fn evaluate(self) -> impl Future<Output = bool>;
}

impl<S: StateItr, F: Future<Output = S>> StateItr for F {
    async fn evaluate(self) -> bool {
        self.await.evaluate().await
    }
}

// State implementation
impl<A: StateItr, B: StateItr> StateItr for K<F2<A, B>> {
    async fn evaluate(self) -> bool {
        match self.0 {
            F2::A(a) => a.evaluate().await,
            F2::B(b) => b.evaluate().await,
        }
    }
}

impl StateItr for K<bool> {
    async fn evaluate(self) -> bool {
        self.0
    }
}

#[instrument]
async fn ksuccess() -> impl StateItr {
    K(true)
}

#[instrument]
async fn kfailure() -> impl StateItr {
    K(false)
}

#[instrument]
async fn state_0() -> impl StateItr {
    ksuccess()
}

#[instrument]
async fn state_1() -> impl StateItr {
    state_0()
}

#[instrument]
async fn state_2() -> impl StateItr {
    if true {
        F2::A(state_1())
    } else {
        F2::B(state_0())
    }
}

#[instrument]
async fn state_3() -> impl StateItr {
    if true {
        F2::A(state_2())
    } else {
        F2::B(state_1())
    }
}

#[instrument]
async fn state_4() -> impl StateItr {
    select_enum!(state_3(), state_1(), state_2(), state_0()).await
}

async fn test_fn() -> bool {
    assert!(state_4().evaluate().await);
    true
}

// macros

pub fn select<A: Future, B: Future>(a: A, b: B) -> future::Or<F2<A, B>, F2<A, B>> {
    future::or(F2::A(a), F2::B(b))
}

#[macro_export]
macro_rules! select_enum {
    ($head:expr $(,)?) => { $head };
    ($head:expr, $($tail:expr),+) => {
        select($head, select_enum!($($tail),+))
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
macro_rules! join {
    ($head:expr) => { $head };
    ($head:expr, $($tail:expr),+ $(,)?) => {
        future::zip($head, join!($($tail),+))
    };
}

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

// State implementation
#[derive(Debug)]
pub enum State {
    Success,
    Failure,
    Running(Pin<Box<dyn FutureEx<Output = State>>>),
}

impl<F: Future<Output = State> + 'static> From<F> for State {
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
    use async_broadcast::{broadcast, Receiver, Sender};
    use core::cell::Cell;
    use core::future::ready;
    use futures_lite::future::block_on;

    fn test_init() {
        let _ = tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
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
        pub topic: Option<(Sender<u32>, Receiver<u32>)>,
    }

    pub trait WorldModel: Clone + Debug + 'static {}

    #[derive(Debug, Default, Clone)]
    pub struct SharedWorldModel(pub Rc<InnerWorldModel>);
    impl WorldModel for SharedWorldModel {}

    #[derive(Debug, Default)]
    pub struct Context<T>(Rc<T>);

    impl<T> Clone for Context<T> {
        fn clone(&self) -> Self {
            Self(self.0.clone())
        }
    }

    use core::ops::Deref;
    impl<T> Deref for Context<T> {
        type Target = T;
        fn deref(&self) -> &T {
            &self.0
        }
    }

    impl Context<InnerWorldModel> {
        /*
        async fn state_a(self) -> State {
            State::Success
        }

        async fn state_b(self) -> State {
            loop {
                if ready(false).await {
                    break self.state_a().into();
                } else if ready(false).await {
                    break self.state_b().into();
                }
                future::yield_now().await
            }
            /*
            if true {
                self.state_a().into()
            } else {
                self.state_a().into()
            }
            */
            /*
            select!(
                transition_if!(ready(false), self.clone().state_a().into()),
                transition_if!(ready(false), self.clone().state_b().into())
            ).await
            */
        }
        */
    }

    impl InnerWorldModel {
        #[instrument(skip(self), ret)]
        pub async fn root(&self) -> bool {
            let (tx, mut rx) = self.topic.clone().unwrap();
            let _ = tx.broadcast(33).await;
            let _ = rx.recv().await;
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

        let topic = Some(broadcast::<u32>(2));

        let ctx = InnerWorldModel {
            topic,
            ..Default::default()
        };

        assert!(block_on(ctx.root()));
        tracing::debug!(?ctx);
    }

    #[test]
    fn static_test() {
        test_init();
        tracing::info!("what");
        assert!(block_on(test_fn()));
    }

    #[test]
    fn cycle_test() {
        test_init();
        cycle_fn();
    }

    #[test]
    fn new_test() {
        test_init();
        tracing::info!("what");
        assert!(block_on(new_test_fn()));
    }
}
