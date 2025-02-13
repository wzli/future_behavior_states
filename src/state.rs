use crate::*;

use alloc::boxed::Box;

pub enum State {
    Running(future::BoxedLocal<State>),
    Success,
    Failure,
}

impl State {
    pub async fn eval(self) -> bool {
        let mut state = self;
        loop {
            match state {
                State::Running(s) => state = s.await,
                State::Success => return true,
                State::Failure => return false,
            }
        }
    }

    pub async fn transition<F: Future<Output = bool>>(self, f: impl Fn() -> F) -> Self {
        if <Repeat>::until(true, f).await {
            self
        } else {
            future::pending().await
        }
    }
}

impl<F: Future<Output = State> + 'static> From<F> for State {
    fn from(f: F) -> Self {
        Self::Running(Box::pin(f))
    }
}

pub trait FutureState: Future<Output = State> + 'static + Sized {
    fn eval(self) -> impl Future<Output = bool> {
        State::from(self).eval()
    }

    fn transition<F: Future<Output = bool>>(
        self,
        f: impl Fn() -> F,
    ) -> impl Future<Output = State> {
        State::from(self).transition(f)
    }
}

impl<F: Future<Output = State> + 'static> FutureState for F {}

#[instrument]
pub fn success() -> State {
    State::Success
}

#[instrument]
pub fn failure() -> State {
    State::Failure
}

#[cfg(test)]
mod tests {
    use super::*;
    use future::block_on;

    #[instrument]
    async fn state0() -> State {
        success()
    }

    #[instrument]
    async fn state1() -> State {
        state0().into()
    }

    #[instrument]
    async fn state2() -> State {
        select!(
            state1().transition(|| state1().eval()),
            success().transition(|| success().eval()),
        )
        .await
    }

    #[instrument]
    async fn dstate0() -> State {
        state2().into()
    }

    #[instrument]
    async fn dstate1() -> State {
        dstate1().into()
    }

    #[instrument]
    async fn dstate2() -> State {
        dstate3().into()
    }

    #[instrument]
    async fn dstate3() -> State {
        dstate3().into()
    }

    #[instrument]
    async fn dstated() -> State {
        select!(
            async { failure() }.transition(|| state1().eval()),
            async { success() },
            async { failure() },
        )
        .await
    }

    #[instrument]
    async fn state_a() -> State {
        state_b().into()
    }

    #[instrument]
    async fn state_b() -> State {
        state_a().into()
    }

    /*
        #[test]
        fn good_test() {
            /*
            let _ = tracing_subscriber::fmt()
                .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
                .with_span_events(
                    tracing_subscriber::fmt::format::FmtSpan::NEW
                        | tracing_subscriber::fmt::format::FmtSpan::CLOSE,
                )
                .with_target(false)
                .try_init();
            */
            assert!(block_on(dstated().eval()));
            assert!(block_on(state_a().eval()));
        }
    */
}
