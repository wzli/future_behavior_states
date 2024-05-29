use core::future::Future;
use core::pin::Pin;
use core::task::Poll;
use futures_lite::{future, FutureExt};
use pin_project::pin_project;
use tracing::instrument;

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

#[cfg(test)]
mod tests {

    use super::*;
    use async_broadcast::{broadcast, Receiver, Sender};
    use core::cell::Cell;
    use core::future::ready;
    use futures_lite::future::block_on;
    use tracing::instrument;

    use crate::tests::test_init;

    async fn test_fn() -> bool {
        assert!(state_4().evaluate().await);
        true
    }

    #[test]
    fn static_test() {
        test_init();
        tracing::info!("what");
        assert!(block_on(test_fn()));
    }
}
