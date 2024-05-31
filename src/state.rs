use futures_lite::{future, FutureExt};
use pin_project::pin_project;
use tracing::instrument;

use alloc::boxed::Box;
use core::future::{ready, Future};
use core::pin::Pin;
use core::task::{Context, Poll};

use crate::repeat_until;
use crate::Behavior;

pub struct DynBehavior(Pin<Box<dyn Future<Output = Pin<Box<dyn Behavior + Unpin>>>>>);

impl Future for DynBehavior {
    type Output = bool;
    fn poll(mut self: Pin<&mut Self>, ctx: &mut core::task::Context<'_>) -> Poll<Self::Output> {
        if let Poll::Ready(mut state) = self.0.poll(ctx) {
            if (*state).as_any().downcast_ref::<Self>().is_none() {
                return state.poll(ctx);
            } else if let Ok(dyn_state) = Pin::into_inner(state).as_box_any().downcast::<Self>() {
                *self = *dyn_state
            }
        }
        ctx.waker().wake_by_ref();
        Poll::Pending
    }
}

pub trait DynWait {
    fn dwait(self) -> DynBehavior
    where
        Self: Sized + 'static;
}

impl<S: Behavior + Unpin + 'static, F: Future<Output = S>> DynWait for F {
    fn dwait(self) -> DynBehavior
    where
        Self: Sized + 'static,
    {
        DynBehavior(Box::pin(async {
            Box::pin(self.await) as Pin<Box<dyn Behavior + Unpin>>
        }))
    }
}

#[macro_export]
macro_rules! select_state {
    ($head:expr) => { $head };
    ($head:expr, $($tail:expr),+ $(,)?) => {
        select($head, select_state!($($tail),+))
    };
}

pub fn select<A: Future, B: Future>(a: A, b: B) -> future::Or<F2<A, B>, F2<A, B>> {
    future::or(F2::A(a), F2::B(b))
}

#[pin_project(project = F2Proj)]
pub enum F2<A, B> {
    A(#[pin] A),
    B(#[pin] B),
}

#[pin_project]
pub struct F<T>(#[pin] T);

impl<A: Future, B: Future> Future for F2<A, B> {
    type Output = F<F2<A::Output, B::Output>>;
    fn poll(self: Pin<&mut Self>, ctx: &mut core::task::Context<'_>) -> Poll<Self::Output> {
        match self.project() {
            F2Proj::A(a) => {
                if let Poll::Ready(x) = a.poll(ctx) {
                    return Poll::Ready(F(F2::A(x)));
                }
            }
            F2Proj::B(b) => {
                if let Poll::Ready(x) = b.poll(ctx) {
                    return Poll::Ready(F(F2::B(x)));
                }
            }
        };
        Poll::Pending
    }
}

impl<O, A: Future<Output = O>, B: Future<Output = O>> Future for F<F2<A, B>> {
    type Output = O;
    fn poll(self: Pin<&mut Self>, ctx: &mut core::task::Context<'_>) -> Poll<O> {
        {
            match self.project().0.project() {
                F2Proj::A(a) => a.poll(ctx),
                F2Proj::B(b) => b.poll(ctx),
            }
        }
    }
}

pub async fn branch_on_result(
    behavior: impl Behavior,
    success_state: impl Future<Output = impl Behavior + 'static>,
    failure_state: impl Future<Output = impl Behavior + 'static>,
) -> impl Behavior {
    if behavior.await {
        F(F2::A(success_state.await))
    } else {
        F(F2::B(failure_state.await))
    }
}

pub async fn check_transition_once(
    behavior: impl Behavior,
    next_state: impl Future<Output = impl Behavior>,
) -> impl Behavior {
    if behavior.await {
        next_state.await
    } else {
        future::pending().await
    }
}

#[macro_export]
macro_rules! transition {
    ($f:expr, $s:expr $(,)?) => {
        check_transition_once(repeat_until!($f, true), $s)
    };
}

#[instrument]
async fn success() -> impl Behavior {
    ready(true)
}

#[instrument]
async fn failure() -> impl Behavior {
    ready(false)
}

#[instrument]
async fn state0() -> impl Behavior {
    success().await
}

#[instrument]
async fn state1() -> impl Behavior {
    state0().await
}

#[instrument]
async fn state2() -> impl Behavior {
    let x = success().await.await;
    select_state!(
        transition!(failure().await.not(), failure()),
        success(),
        failure(),
        success(),
        failure(),
        //transition(success().await, success(), failure())
    )
    .await
}

#[instrument]
async fn dstate0() -> DynBehavior {
    state2().dwait()
}

#[instrument]
async fn dstate1() -> DynBehavior {
    dstate1().dwait()
}

#[instrument]
async fn dstate2() -> impl Behavior {
    //state0().await
    dstate3().await
}

#[instrument]
async fn dstate3() -> DynBehavior {
    dstate3().dwait()
}

#[instrument]
async fn dstated() -> DynBehavior {
    select_state!(
        transition!(success().await.not(), failure()),
        success(),
        failure(),
        success(),
        failure(),
        //transition(success().await, success(), failure()),
    )
    .dwait()
}

#[instrument]
async fn state_a() -> impl Behavior {
    state_b().await
}

#[instrument]
async fn state_b() -> DynBehavior {
    state_a().dwait()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::test_init;
    use futures_lite::future::block_on;

    #[test]
    fn good_test() {
        test_init();
        //assert!(block_on(state2().dwait()));
        //assert!(block_on(dstate3().dwait()));
        //assert!(block_on(dstate2().dwait()));
        assert!(block_on(dstated().dwait()));
    }
}
