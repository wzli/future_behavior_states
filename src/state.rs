use crate::FutureEx;
use alloc::boxed::Box;
use core::fmt::Debug;
use core::future::{pending, Future};
use core::pin::Pin;
use core::task::Poll;
use futures_lite::FutureExt;
use tracing::instrument;

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

    use core::future::ready;

    use futures_lite::future;
    use tracing::instrument;

    use crate::tests::test_init;

    #[derive(Debug, Clone)]
    pub struct Context<T>(Rc<T>);

    use core::ops::Deref;
    impl<T> Deref for Context<T> {
        type Target = T;
        fn deref(&self) -> &T {
            &self.0
        }
    }

    impl<T: 'static> Context<T> {
        #[instrument(skip(self))]
        pub async fn state_a(self) -> State {
            self.state_b().into()
        }

        #[instrument(skip(self))]
        pub async fn state_b(self) -> State {
            self.state_c().into()
        }

        #[instrument(skip(self))]
        pub async fn state_c(self) -> State {
            select!(
                transition(ready(false), None, None),
                transition(ready(true), Some(State::Success), None),
                transition(ready(true), Some(State::Success), None),
                transition(ready(true), Some(State::Success), None),
                transition_if!(ready(true).not(), State::Success),
                transition_if!(ready(true).not(), self.state_c().into()),
            )
            .await
        }

        // choose a tune to hum
        #[instrument(skip(self), ret)]
        pub async fn choose_tune(self) -> bool {
            select!(ready(true), ready(true), ready(true)).await
            // (X(hum_a_tune(wm.clone(), 2)) | X(hum_a_tune(wm, 3)) ).0.await
            // (X(hum_a_tune(wm.clone(), 2)) | X(hum_a_tune(wm.clone(), 3)) | X(hum_a_tune(wm, 3))) .0 .await
            //((X(hum_a_tune(wm.clone(), 2)) | X(hum_a_tune(wm, 3))) | X(hum_a_tune(wm, 4))).0.await
        }

        #[instrument(skip(self), ret)]
        pub async fn hum_a_tune(self, id: u8) -> bool {
            false
        }
    }

    #[test]
    fn state_machine_test() {
        test_init();
        let ctx = Context(Rc::new(0));
        let root = State::from(ctx.state_a());
        let ret = futures_lite::future::block_on(root);
        tracing::debug!(ret);
    }
}
