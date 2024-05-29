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
    
    
    use futures_lite::{future};
    use tracing::instrument;

    use crate::tests::test_init;

    pub trait WorldModel: Clone + Debug + 'static {}

    #[derive(Debug, Default, Clone)]
    pub struct SharedWorldModel(pub Rc<u8>);
    impl WorldModel for SharedWorldModel {}

    #[derive(Debug)]
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

    #[test]
    fn state_machine_test() {
        test_init();
        let wm = SharedWorldModel::default();
        let root = State::from(state_a(wm));
        let ret = futures_lite::future::block_on(root);
        tracing::debug!(ret);
    }
}
