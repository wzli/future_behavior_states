use crate::*;

pub enum State {
    Running(future::BoxedLocal<State>),
    Success,
    Failure,
}

impl<F: Future<Output = State> + 'static> From<F> for State {
    fn from(f: F) -> Self {
        Self::Running(alloc::boxed::Box::pin(f))
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

    #[test]
    fn state_eval() {
        async fn a() -> State {
            success().into()
        }

        async fn b() -> State {
            a().into()
        }

        async fn c() -> State {
            b().into()
        }

        assert!(block_on(c().eval()));
    }

    #[test]
    fn state_when() {
        async fn x() -> State {
            select!(x().when(|| ready(false)), success().when(|| ready(true)),).into()
        }
        assert!(block_on(x().eval()));
    }

    #[test]
    fn result_when_eval() {
        async fn x() -> State {
            select!(
                x().when(|| ready(false)),
                result_when(true, false, || ready(true)),
            )
            .into()
        }
        assert!(block_on(x().eval()));
    }
}
