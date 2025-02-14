use future_behavior_states::*;
use futures_lite::future::block_on;

use core::cell::Cell;
use std::rc::Rc;

fn test_init() {
    let _ = tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_span_events(tracing_subscriber::fmt::format::FmtSpan::NEW)
        .with_target(false)
        .try_init();
}

#[derive(Debug, Default)]
struct Context {
    pub counter: Cell<u32>,
}

impl Context {
    #[instrument(skip(self))]
    pub async fn state_0(self: Rc<Self>) -> State {
        success().into()
    }

    #[instrument(skip(self))]
    pub async fn state_1(self: Rc<Self>) -> State {
        self.state_0().into()
    }

    #[instrument(skip(self))]
    pub async fn state_2(self: Rc<Self>) -> State {
        self.state_1().into()
    }

    #[instrument(skip(self))]
    pub async fn state_3(self: Rc<Self>) -> State {
        if self.clone().count_upto(10).await {
            self.state_0().into()
        } else {
            self.state_2().into()
        }
    }

    #[instrument(skip(self))]
    pub async fn state_4(self: Rc<Self>) -> State {
        self.state_3().into()
    }

    #[instrument(ret)]
    pub async fn count_upto(self: Rc<Self>, count: u32) -> bool {
        let x = self.counter.get();
        self.counter.set(x + 1);
        x + 1 >= count
    }

    #[instrument(skip(self))]
    pub async fn state_transitions_0(self: Rc<Self>) -> State {
        loop {
            if ready(false).await {
                return failure().into();
            } else if ready(true).await {
                return self.state_transitions_1().into();
            }
            future::yield_now().await
        }
    }

    #[instrument(skip(self))]
    pub async fn state_transitions_1(self: Rc<Self>) -> State {
        loop {
            if ready(false).await {
                return failure().into();
            } else if ready(true).await {
                return self.state_transitions_2().into();
            }
            future::yield_now().await
        }
    }

    #[instrument(skip(self))]
    pub async fn state_transitions_2(self: Rc<Self>) -> State {
        loop {
            if ready(false).await {
                return failure().into();
            } else if ready(true).await {
                return success().into();
            }
            future::yield_now().await
        }
    }

    #[instrument(skip(self))]
    pub async fn state_transitions_parallel_0(self: Rc<Self>) -> State {
        select!(
            failure().when(|| ready(false)),
            self.state_transitions_parallel_1().when(|| ready(true)),
        )
        .into()
    }

    #[instrument(skip(self))]
    pub async fn state_transitions_parallel_1(self: Rc<Self>) -> State {
        select!(
            failure().when(|| ready(false)),
            self.state_transitions_parallel_2().when(|| ready(true)),
        )
        .into()
    }

    #[instrument(skip(self))]
    pub async fn state_transitions_parallel_2(self: Rc<Self>) -> State {
        select!(
            failure().when(|| ready(false)),
            success().when(|| ready(true)),
        )
        .into()
    }
}

#[test]
fn state_machine_chain() {
    test_init();
    let ctx = Rc::new(Context::default());
    assert!(block_on(ctx.clone().state_4().eval()));
    tracing::debug!(?ctx);
}

#[test]
fn state_machine_transitions() {
    test_init();
    let ctx = Rc::new(Context::default());
    assert!(block_on(ctx.clone().state_transitions_0().eval()));
}

#[test]
fn state_machine_transitions_parallel() {
    test_init();
    let ctx = Rc::new(Context::default());
    assert!(block_on(ctx.clone().state_transitions_parallel_0().eval()));
}
