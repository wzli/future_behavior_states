use alloc::boxed::Box;
use core::future::Future;
use core::pin::Pin;
use core::task::Poll;
use futures_lite::{future, FutureExt};
use pin_project::pin_project;
use tracing::instrument;
use futures_lite::future::pending;
use core::future::ready;
use crate::FutureEx;

#[macro_export]
macro_rules! transition_iff {
    ($f:expr, $s:expr $(,)?) => {
        transition(repeat_until!($f, true), Some($s), None::<Pin<Box<dyn Future<Output=StateDyn>>>>)
    };
}

#[macro_export]
macro_rules! select_enum {
    ($head:expr) => { $head };
    ($head:expr, $($tail:expr),+ $(,)?) => {
        select($head, select_enum!($($tail),+))
    };
}

pub fn select<A: Future, B: Future>(a: A, b: B) -> future::Or<F2<A, B>, F2<A, B>> {
    future::or(F2::A(a), F2::B(b))
}

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
use core::any::Any;

pub trait StateItr {
    fn evaluate(self) -> impl Future<Output = bool>;
}

pub trait DynStateItr {
    fn eval(&mut self) -> Pin<Box<dyn Future<Output = bool> + '_>>;
    fn as_any(&mut self) -> &mut dyn Any;
}

struct StateDyn(Box<dyn DynStateItr>);

impl StateItr for K<bool> {
    async fn evaluate(self) -> bool {
        self.0
    }
}


impl<S: StateItr, F: Future<Output = S>> StateItr for F {
    async fn evaluate(self) -> bool {
        self.await.evaluate().await
    }
}

impl StateItr for StateDyn {
    async fn evaluate(mut self) -> bool {
        // maybe can loop here instead of recursion?
        // compare type id of dyn DynStateItr to see it it matches success or failure
        self.0.eval().await
    }
}

impl<S: StateItr, F: Future<Output = S> + 'static> DynStateItr for Option<F> {
    fn eval(&mut self) -> Pin<Box<dyn Future<Output = bool> + '_>> {
        Box::pin( async{
            if let Some(x) = core::mem::take(self) {
                x.await.evaluate().await
            } else {
                false
            }
        })
    }
    fn as_any(&mut self) -> &mut dyn Any {
        self
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

pub async fn transition<A: StateItr, B: StateItr>(
    f: impl Future<Output = bool>,
    success_state: Option<A>,
    failure_state: Option<B>,
) -> K<F2<A, B>> {
    if f.await {
        if let Some(state) = success_state {
            return K(F2::A(state));
        }
    } else if let Some(state) = failure_state {
        return K(F2::B(state));
    }
    pending().await
}

#[instrument]
async fn dstate_a() -> StateDyn {
    //Box::new(StateDyn::from(dstate_a()))
    //Box::new(Some(dstate1()))
    StateDyn(Box::new(Some(dstate_a())))
    //StateDyn(Box::new(Some(success())))
}

#[instrument]
async fn dstate1() -> impl StateItr {
    dstate_a()
}

#[instrument]
async fn dstate2() -> impl StateItr {
    dstate1()
}

#[instrument]
async fn success() -> impl StateItr {
    K(true)
}

#[instrument]
async fn failure() -> impl StateItr {
    K(false)
}

/*
#[instrument]
async fn dstate0() -> StateDyn {
    select!(success(), success(),
        //transition(ready(true), Some(success()), Some(success())).map(Into::into),
        //transition_iff!(ready(true), success()).map(Into::into),
    ).await
}


#[instrument]
async fn state_0() -> impl StateItr {
    success()
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
    select_enum!(state_3(), state_1(), state_2(), state_0(),
        //transition_iff!(ready(true), success())
    ).await
}
*/

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::test_init;
    use futures_lite::future::block_on;

    /*
    #[test]
    fn static_test() {
        test_init();
        assert!(block_on(state_4().evaluate()));
    }
    */

    #[test]
    fn dyn_test() {
        test_init();
        assert!(block_on(async {dstate_a().await.0.eval().await }));
    }
}
