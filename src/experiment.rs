use tracing::instrument;

struct StateFuture<T>(fn(&T) -> StateFuture<T>);

impl<T> StateFuture<T> {
    async fn fut(&self, t: &T) -> Self {
        self.0(t)
    }

    #[instrument(skip(self, t), ret)]
    async fn evaluate(&self, t: &T) -> bool {
        let mut state = self.fut(t).await;
        loop {
            #[allow(clippy::fn_address_comparisons)]
            if state.0 == success {
                return true;
            } else if state.0 == failure {
                return false;
            } else {
                state = state.fut(t).await;
            }
        }
        /*
        loop {
            match state.0 {
                success => return true,
                failure => return false,
                _ => {state = state.fut(t).await; }
            }
        }

        while state.0 != terminal_state {
            state = state.fut(t).await;
        }
        let mut state = self.fut(t).await;
        loop {
            match  state {
                Ok(next_state) => { state = next_state.fut(t).await; },
                Err(result) => return result,
            }
        }
        */
    }
}

#[instrument(skip(t))]
fn success<T>(t: &T) -> StateFuture<T> {
    StateFuture(success)
}

#[instrument(skip(t))]
fn failure<T>(t: &T) -> StateFuture<T> {
    StateFuture(failure)
}

struct Ctx;

impl Ctx {
    #[instrument(skip(self))]
    fn state_fut_0(&self) -> StateFuture<Self> {
        StateFuture(Self::state_fut_1)
    }

    #[instrument(skip(self))]
    fn state_fut_1(&self) -> StateFuture<Self> {
        StateFuture(Self::state_fut_2)
    }

    #[instrument(skip(self))]
    fn state_fut_2(&self) -> StateFuture<Self> {
        StateFuture(success)
        //StateFuture(Self::state_fut_0)
    }
}

async fn new_test_fn() -> bool {
    let ctx = Ctx;
    ctx.state_fut_0().evaluate(&ctx).await

    /*
    for _ in 0..10 {
        s = s.fut(&ctx).await;
        if s.0 == Ctx::state_fut_0 {
            break;
        }
    }
    */
}

pub trait StateFn {
    fn fut(self) -> impl StateFn;
}

trait ST<T> {
    fn call(self) -> T;
}

struct S<T>(fn() -> T);

impl<O, T: ST<O>> ST<O> for S<T> {
    fn call(self) -> O {
        self.0().call()
    }
}

impl<T> StateFn for S<T>
where
    T: StateFn,
{
    fn fut(self) -> impl StateFn {
        self.0()
    }
}

struct X(S<S<S<S<X>>>>);

struct Y<S: ST<Y<S>>>(S);

struct Z(fn() -> Z);

struct W(S<W>);

struct W2(S<S<W2>>);

impl StateFn for Z {
    fn fut(self) -> impl StateFn {
        self.0()
    }
}

impl StateFn for W {
    fn fut(self) -> impl StateFn {
        self.0 .0()
    }
}

impl StateFn for W2 {
    fn fut(self) -> impl StateFn {
        self.0 .0()
    }
}

pub struct K<T>(T);
impl StateFn for K<bool> {
    fn fut(self) -> impl StateFn {
        self
    }
}

#[instrument]
fn state_fn_0() -> impl StateFn {
    //W(S(state_fn_0))
    //WA(S(state_fn_1))
    //S(state_fn_0)
    K(false)
}
/*

#[instrument]
fn state_fn_1() -> impl StateFn {
    //S(state_fn_0)
    //W(S(state_fn_1))
    WB(S(state_fn_0))
}

#[instrument]
async fn state_fn_2() -> impl StateFn {
    state_fn_1
}
*/

/*
struct X;
impl StateFn for X {
    type A = ();
}

fn what() -> impl StateFn {
    X
}
*/

#[cfg(test)]
mod tests {

    use super::*;

    use futures_lite::future::block_on;

    use crate::tests::test_init;

    #[test]
    fn new_test() {
        test_init();
        tracing::info!("what");
        assert!(block_on(new_test_fn()));
    }
}
