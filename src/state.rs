use alloc::boxed::Box;
use core::pin::Pin;
use core::task::{Context, Poll};

use crate::*;

pub struct DynBehavior(pub Pin<Box<dyn Future<Output = InnerBehavior>>>);
pub type InnerBehavior = Pin<Box<dyn Behavior + Unpin>>;

impl Future for DynBehavior {
    type Output = bool;
    fn poll(mut self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        tracing::debug!("poll");
        if let Poll::Ready(mut state) = self.0.poll(ctx) {
            if (*state).as_any().downcast_ref::<Self>().is_none() {
                tracing::debug!("other");
                return state.poll(ctx);
            } else if let Ok(dyn_state) = Pin::into_inner(state).as_box_any().downcast::<Self>() {
                tracing::debug!("self");
                *self = *dyn_state
            }
        }
        tracing::debug!("end");
        ctx.waker().wake_by_ref();
        Poll::Pending
    }
}

pub trait State: Sized {
    fn eval(self) -> impl Behavior;

    fn into_dyn(self) -> DynBehavior
    where
        Self: 'static;
}

impl<S: Behavior + Unpin + 'static, F: Future<Output = S>> State for F {
    fn eval(self) -> impl Behavior {
        async { self.await.await }
    }

    fn into_dyn(self) -> DynBehavior
    where
        Self: 'static,
    {
        tracing::debug!("into");
        DynBehavior(Box::pin(async { Box::pin(self.await) as InnerBehavior }))
    }
}

pub async fn branch_on_result(
    behavior: impl Behavior,
    success_state: impl Future<Output = impl Behavior + 'static>,
    failure_state: impl Future<Output = impl Behavior + 'static>,
) -> impl Behavior {
    if behavior.await {
        Either::A(success_state.await)
    } else {
        Either::B(failure_state.await)
    }
}

#[instrument]
pub async fn success() -> impl Behavior {
    ready(true)
}

#[instrument]
pub async fn failure() -> impl Behavior {
    ready(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_lite::future::block_on;

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
        select_state!(
            transition!(failure().await.not(), failure().await),
            success(),
            failure(),
            success(),
            failure(),
        )
        .await
    }

    #[instrument]
    async fn dstate0() -> DynBehavior {
        state2().into_dyn()
    }

    #[instrument]
    async fn dstate1() -> DynBehavior {
        dstate1().into_dyn()
    }

    #[instrument]
    async fn dstate2() -> impl Behavior {
        dstate3().await
    }

    #[instrument]
    async fn dstate3() -> DynBehavior {
        dstate3().into_dyn()
    }

    #[instrument]
    async fn dstated() -> DynBehavior {
        select_state!(
            transition!(success().await.not(), failure().await),
            success(),
            failure(),
            success(),
            failure(),
        )
        .into_dyn()
    }

    #[instrument]
    async fn state_a() -> impl Behavior {
        state_b().await
    }

    #[instrument]
    async fn state_b() -> DynBehavior {
        state_a().into_dyn()
    }

    #[test]
    fn good_test() {
        assert!(block_on(dstated().into_dyn()));
        //assert!(block_on(state_a().into_dyn()));
    }
}
