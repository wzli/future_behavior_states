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
    pub async fn state_0(&self) -> impl Behavior {
        success().await
    }

    #[instrument(skip(self))]
    pub async fn state_1(&self) -> impl Behavior {
        self.state_0().await
    }

    #[instrument(skip(self))]
    pub async fn state_2(self: Rc<Self>) -> impl Behavior {
        self.state_1().await
    }

    #[instrument(skip(self))]
    pub async fn state_3(self: Rc<Self>) -> DynBehavior {
        branch_on_result(
            self.clone().count_upto(10),
            self.clone().state_2(),
            self.clone().state_4(),
        )
        .into_dyn()
    }

    #[instrument(skip(self))]
    pub async fn state_4(self: Rc<Self>) -> impl Behavior {
        self.clone().state_3().await
    }

    #[instrument(ret)]
    pub async fn count_upto(self: Rc<Self>, count: u32) -> bool {
        let x = self.counter.get();
        self.counter.set(x + 1);
        x + 1 >= count
    }

    #[instrument(skip(self))]
    pub async fn state_sel(self: Rc<Self>) -> impl Behavior {
        //self.state_0().await
        select_state!(self.state_0(), self.state_1()).await
    }
}

#[test]
fn state_machine_tests() {
    test_init();

    let ctx = Rc::new(Context::default());

    assert!(block_on(ctx.clone().state_4().eval()));
    tracing::debug!(?ctx);
}
