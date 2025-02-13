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
    pub async fn state_0(&self) -> bool {
        true
    }

    #[instrument(skip(self))]
    pub async fn state_1(&self) -> bool {
        self.state_0().await
    }

    #[instrument(skip(self))]
    pub async fn state_2(self: Rc<Self>) -> bool {
        self.state_1().await
    }

    #[instrument(skip(self))]
    pub async fn state_3(self: Rc<Self>) -> bool {
        if self.clone().count_upto(10).await {
            self.clone().state_0().await
        } else {
            self.clone().state_2().await
        }
        /*
        self.clone().count_upto(10).branch(
            self.clone().state_0(),
            self.clone().state_2(),
        ).await.await
        */
    }

    #[instrument(skip(self))]
    pub async fn state_4(self: Rc<Self>) -> bool {
        self.clone().state_3().await
    }

    #[instrument(ret)]
    pub async fn count_upto(self: Rc<Self>, count: u32) -> bool {
        let x = self.counter.get();
        self.counter.set(x + 1);
        x + 1 >= count
    }

    #[instrument(skip(self))]
    pub async fn state_sel(self: Rc<Self>) -> bool {
        //self.state_0().await
        select!(self.state_0(), self.state_1()).await
    }

    #[instrument(skip(self))]
    pub async fn state_transitions_parallel(&self) -> bool {
        //self.state_0().await
        select_state!(
            transition!(self.state_0() => self.state_1()),
            transition!(self.state_1() => self.state_0()),
        )
        .await
        .await
    }

    #[instrument(skip(self))]
    pub async fn state_transitions(&self) -> bool {
        loop {
            if self.state_0().await {
                return self.state_0().await;
            } else if self.state_0().await {
                return self.state_1().await;
            }
            future::yield_now().await
        }
    }
}

#[test]
fn state_machine_tests() {
    test_init();

    let ctx = Rc::new(Context::default());

    assert!(block_on(ctx.clone().state_4()));
    tracing::debug!(?ctx);
}
