use crate::*;

use alloc::boxed::Box;

pub enum State {
    Running(future::BoxedLocal<State>),
    Success,
    Failure,
}

/*
impl<FB: Future<Output = bool>, FS: Future<Output = State>> From<FB> for FS {
    fn from(b: FS) -> FS {
        b.map(State::Success)
    }
}
*/

impl<F: Future<Output = State> + 'static> From<F> for State {
    fn from(f: F) -> Self {
        Self::Running(Box::pin(f))
    }
}

pub trait FutureState: Future<Output = State> + 'static + Sized {
    fn eval(self) -> impl Future<Output = bool> {
        async {
            let mut state = self.await;
            loop {
                match state {
                    State::Running(s) => state = s.await,
                    State::Success => return true,
                    State::Failure => return false,
                }
            }
        }
    }

    fn when<F: Future<Output = bool>>(self, f: impl Fn() -> F) -> impl Future<Output = State> {
        async move {
            loop {
                if f().await {
                    return self.into();
                }
                future::yield_now().await
            }
        }
    }
}

impl<F: Future<Output = State> + 'static> FutureState for F {}

pub async fn result_when<F: Future<Output = bool>>(
    return_success: bool,
    return_failure: bool,
    f: impl Fn() -> F,
) -> State {
    loop {
        if f().await {
            if return_success {
                return State::Success;
            }
        } else if return_failure {
            return State::Failure;
        }
        future::yield_now().await
    }
}

#[instrument]
pub async fn success() -> State {
    State::Success
}

#[instrument]
pub async fn failure() -> State {
    State::Failure
}

#[cfg(test)]
mod tests {
    use super::*;
    use future::block_on;

    #[instrument]
    async fn state0() -> State {
        success().into()
    }

    #[instrument]
    async fn state1() -> State {
        state0().into()
    }

    #[instrument]
    async fn state2() -> State {
        select!(
            state1().when(|| state1().eval()),
            success().when(|| success().eval()),
            result_when(false, false, || state1().eval()),
        )
        .into()
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
        select!(failure().when(|| state1().eval()), success(), failure(),).into()
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
