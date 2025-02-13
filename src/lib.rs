#![no_std]
#![allow(clippy::async_yields_async)]

extern crate alloc;

pub use core::future::{ready, Future};
pub use futures_lite::{future, FutureExt};
pub use tracing::instrument;

pub use behavior::*;
pub use state::*;

use core::any::{Any, TypeId};
use core::pin::Pin;
use core::task::{Context, Poll};
use pin_project::pin_project;

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

#[macro_export]
macro_rules! select_state {
    ($head:expr $(,)?) => { $head };
    ($head:expr, $($tail:expr),+ $(,)?) => {
        select_state($head, select_state!($($tail),+))
    };
}

pub fn select_state<A: Future, B: Future>(
    a: A,
    b: B,
) -> future::Or<Selection<A, B>, Selection<A, B>> {
    future::or(Selection::A(a), Selection::B(b))
}

#[pin_project(project = SelectionProj)]
pub enum Selection<A, B> {
    A(#[pin] A),
    B(#[pin] B),
}

impl<A: Future, B: Future> Future for Selection<A, B> {
    type Output = Either<A::Output, B::Output>;
    fn poll(self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.project() {
            SelectionProj::A(a) => {
                if let Poll::Ready(x) = a.poll(ctx) {
                    return Poll::Ready(Either::A(x));
                }
            }
            SelectionProj::B(b) => {
                if let Poll::Ready(x) = b.poll(ctx) {
                    return Poll::Ready(Either::B(x));
                }
            }
        };
        Poll::Pending
    }
}

#[pin_project(project = EitherProj)]
pub enum Either<A, B> {
    A(#[pin] A),
    B(#[pin] B),
}

impl<O, A: Future<Output = O>, B: Future<Output = O>> Future for Either<A, B> {
    type Output = O;
    fn poll(self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<O> {
        {
            match self.project() {
                EitherProj::A(a) => a.poll(ctx),
                EitherProj::B(b) => b.poll(ctx),
            }
        }
    }
}

// Future trait extensions
pub trait FutureEx: Future + Sized {
    fn map<O>(self, f: impl FnOnce(<Self as Future>::Output) -> O) -> impl Future<Output = O>
    {
        async { f(self.await) }
    }

    fn force<O>(self, o: O) -> impl Future<Output = O>
    {
        self.map(move |_| o)
    }

    fn not(self) -> impl Future<Output = bool>
    where Self: Future<Output = bool>
    {
        async { !self.await }
    }

    fn then<B>(self, b: B) -> impl Future<Output = B>
    where Self: Future<Output = bool>
    {
        async {
            if self.await {
                b
            } else {
                future::pending().await
            }
        }
    }

    /*
    fn eval(self) -> impl Future<Output = bool>
    where
        Self: Future<Output = state::State>,
    {
        async { self.await.eval().await }
    }

    fn branch<A, B>(self, a: A, b: B) -> impl Future<Output = Either<A, B>>
    where
        Self: Future<Output = bool> + Sized
    {
        async {
            if self.await {
                Either::A(a)
            } else {
                Either::B(b)
            }
        }
    }
    */
}

impl<F: Future> FutureEx for F {}

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

mod state {

    use crate::*;

    use alloc::boxed::Box;
    struct State(future::BoxedLocal<bool>);

    /*
    impl Future for State {
        type Output = bool;
        fn poll(mut self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
            self.0.poll(ctx)
        }
    }
    */

    /*
    impl<F: Future<Output=State> + 'static> From<F> for State {
        fn from(f: F) -> Self {
            Self(Box::pin(f))
        }
    }
    */

    /*
    #[instrument]
    pub async fn success() -> State {
        xsuccess().into()
    }

    #[instrument]
    pub async fn xfailure() -> State {
        xfailure().into()
    }
    */

}


/*
// mod state;

use alloc::boxed::Box;

//type wtf = Result<bool, Dstate>;

//struct Dstate(Pin<Box<dyn Future<Output = Result<bool, Dstate>>>>);
struct Dstate(Result<bool, BoxedLocal<Dstate>>);
impl Dstate {
    fn defer(self) -> Dstate {
        async { self }.into()
    }
}
//type Rstate = Result<bool, BoxedLocal<Dstate>>;

use core::any::{Any, TypeId, type_name};

fn get_id<T: 'static>(_: fn() -> T) -> TypeId {
    TypeId::of::<T>()
}

struct Xstate(Pin<Box<dyn State>>);

trait State: Future<Output = Xstate> + 'static{
    fn id(&self) -> TypeId
    {
        self.type_id()
    }

    async fn eval(self) -> bool
    where
        Self: Sized,
    {
        let mut state = self.await;
        loop {
            if (*state.0).id() == get_id(xsuccess) {
                break true
            } else if (*state.0).id() == get_id(xfailure) {
                break false
            } else {
                state = state.0.await
            }
        }
    }
}
impl<S: Future<Output = Xstate> + 'static> State for S {}

impl<F: State> From<F> for Xstate {
    fn from(f: F) -> Self {
        Self(Box::pin(f))
    }
}

#[instrument]
pub async fn xsuccess() -> Xstate {
    xsuccess().into()
}

#[instrument]
pub async fn xfailure() -> Xstate {
    xfailure().into()
}

trait D_state: Future<Output = Dstate> + Sized {
    async fn eval(self) -> bool {
        let mut x = self.await;
        loop {
            match x.0 {
                Ok(b) => break b,
                Err(f) => x = f.await,
            }
        }
    }

    async fn map_result(self, f: impl Fn(bool) -> bool + 'static) -> Dstate
    where
        Self: 'static,
    {
        match self.await.0 {
            Ok(result) => Dstate(Ok(f(result))),
            Err(state) => Dstate(Err(state.map_result(f).boxed_local())).defer(),
        }
    }

    async fn combine_result(
        self,
        rhs: impl Future<Output = Dstate>,
        f: impl Fn(bool, bool) -> bool + 'static,
    ) -> Dstate {
        match self.await.0 {
            Ok(result_a) => match rhs.await.0 {
                Ok(result_b) => Dstate(Ok(f(result_a, result_b))),
                Err(state_b) => {
                    state_b
                        .map_result(move |result_b| f(result_a, result_b))
                        .await
                }
            },
            Err(state_a) => match rhs.await.0 {
                Ok(result_b) => {
                    state_a
                        .map_result(move |result_a| f(result_a, result_b))
                        .await
                }
                Err(state_b) => {
                    Dstate(Err(state_a.combine_result(state_b, f).boxed_local())).defer()
                }
            },
        }
    }

    async fn not(self) -> Dstate
    where
        Self: 'static,
    {
        self.map_result(|x| !x).await
    }

    async fn and(self, rhs: impl Future<Output = Dstate>) -> Dstate {
        self.combine_result(rhs, |a, b| a && b).await
    }

    async fn nand(self, rhs: impl Future<Output = Dstate>) -> Dstate {
        self.combine_result(rhs, |a, b| !(a && b)).await
    }

    async fn or(self, rhs: impl Future<Output = Dstate>) -> Dstate {
        self.combine_result(rhs, |a, b| a || b).await
    }

    async fn nor(self, rhs: impl Future<Output = Dstate>) -> Dstate {
        self.combine_result(rhs, |a, b| !(a || b)).await
    }

    async fn eq(self, rhs: impl Future<Output = Dstate>) -> Dstate {
        self.combine_result(rhs, |a, b| a == b).await
    }

    async fn ne(self, rhs: impl Future<Output = Dstate>) -> Dstate {
        self.combine_result(rhs, |a, b| a != b).await
    }
}

impl<S: Future<Output = Dstate> + Sized> D_state for S {}

impl<F: Future<Output=bool>> From<Dstate> for F {
    fn from(s: Dstate) ->  F {
        s.eval()
    }
}

use core::ops::Deref;

impl Deref for Dstate{
    type Target = bool;

    fn deref(&self) -> &Self::Target {
        &self.
    }
}

impl Future for Dstate {
    type Output = bool;
    fn poll(mut self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.0 {
            Ok(result) => return Poll::Ready(result),
            Err(ref mut next) => {
                if let Poll::Ready(state) = next.poll(ctx) {
                    *self = state;
                    ctx.waker().wake_by_ref();
                }
            }
        }
        Poll::Pending
    }
}

impl Into<bool> for Dstate {
    fn Into(self) -> impl Future<bool> {
        await { self.await.await }
    }
}

impl<F: Future<Output = Dstate> + 'static> From<F> for Dstate {
    fn from(f: F) -> Self {
        Self(Err(f.boxed_local()))
    }
}

#[instrument]
pub async fn state0() -> bool {
    state1().await
}

#[instrument]
pub async fn state1() -> bool {
    state2().await
}

#[instrument]
pub async fn state2() -> bool {
    state3().await
}

#[instrument]
pub async fn state3() -> bool {
    true
}

#[instrument]
pub async fn state_d() -> bool {
    //state_d().boxed_local().await
    //Dstate(Box::pin(async { Ok(true) } )).await
    //Dstate(state_d().map(|x| Either::B(x)).boxed_local())
    true
}

#[instrument]
pub async fn state_dd() -> Xstate {
    //state_e().and(state_e()).await
    state_dd().into()
    //xsuccess().await
    //xfailure().into()
    //xfailure().await
    //Dstate(Box::pin(state0().map(|x| Ok(x))))
    /*
    (select_state!(
        to_c(state_dd()).then(state1()),
        state2().then(state3()),
        state2().branch(state1(), state3()),
        transition!(state0() => state1()),
    )
    .await).await
    */
}

#[instrument]
pub async fn state_e() -> Xstate {
    /*
    Dstate(Err(
            async {
                Dstate(
                    match state_dd().await.0 {
                        Ok(res) => Ok(!res),
                        Err(state) => Err(state),
                    }
                )

            } .boxed_local()
    ))
    */
    state_dd().await
}

#[instrument]
pub async fn dsuccess() -> Dstate {
    Dstate(Ok(true))
}

#[instrument]
pub async fn dfailure() -> Dstate {
    Dstate(Ok(true))
}

#[derive(Debug)]
struct Ctx;

#[instrument]
//fn state_x(ctx: &Ctx) -> Pin<Box<dyn Future<Output = bool> + Send + '_>> {
fn state_x(ctx: &Ctx) -> Boxed<bool> {
    /*
    let x = state_x(ctx).boxed();
    let y = state_x(ctx).boxed();
    x
    */
    async {
        select_state!(
            state0().then(state1()),
            state2().then(state3()),
            state2().branch(state1(), state3()),
            transition!(state0() => state1()),
        )
        .await
        .await
    }
    .boxed()

    //as Pin<Box<dyn Future<Output=bool>>>
}

#[instrument]
pub async fn state_z(ctx: &Ctx) -> bool {
    select_state!(
        state_z(ctx).boxed_local().then(state1()),
        state2().then(state3()),
        state2().branch(state1(), state3()),
        transition!(state0() => state1()),
    )
    .await
    .await
}

#[instrument]
pub async fn state_y() -> DynBehavior {
    tracing::debug!("inside");
    state_y().into_dyn()
}

#[instrument]
pub async fn state_sel() -> bool {
    select_state!(state0().then(state1()), state2().then(state3()),)
        .await
        .await
}

#[instrument]
pub async fn state_sel2() -> bool {
    select_state!(
        state0().then(state1()),
        state2().then(state3()),
        state2().branch(state1(), state3()),
        transition!(state0() => state1()),
    )
    .await
    .await
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_lite::future::block_on;

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
}
*/
