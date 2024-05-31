use future_behavior_states::*;
use futures_lite::future::block_on;

use async_broadcast::{broadcast, Receiver, Sender};
use core::cell::Cell;

fn test_init() {
    let _ = tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_span_events(tracing_subscriber::fmt::format::FmtSpan::NEW)
        .with_target(false)
        .try_init();
}

#[derive(Debug, Default)]
struct Context {
    pub enemy_near: Cell<bool>,
    pub moved_to_enemy: Cell<bool>,
    pub attacked: Cell<bool>,
    pub is_on_grass: Cell<bool>,
    pub sat_down: Cell<bool>,
    pub waited: Cell<bool>,
    pub hummed: Cell<u8>,
    pub topic: Option<(Sender<u32>, Receiver<u32>)>,
}

impl Context {
    #[instrument(skip(self), ret)]
    pub async fn root(&self) -> bool {
        let (tx, mut rx) = self.topic.clone().unwrap();
        let _ = tx.broadcast(33).await;
        let _ = rx.recv().await;
        self.handle_enemy().await
            || self.chill_on_grass().await
            || self.choose_tune().await
            || self.choose_tune2().await
    }

    // choose a tune to hum
    pub async fn choose_tune(&self) -> bool {
        parallel_any!(self.hum_a_tune(2), self.hum_a_tune(1)).await
    }

    pub async fn choose_tune2(&self) -> bool {
        //try_zip!(ready(true), ready(true)).await
        parallel_all!(ready(true), ready(true)).await
    }

    #[instrument(skip(self), ret)]
    pub async fn hum_a_tune(&self, id: u8) -> bool {
        if self.hummed.get() != 0 {
            return false;
        }
        self.hummed.set(id);
        true
    }

    // handle enemy
    #[instrument(skip(self), ret)]
    pub async fn handle_enemy(&self) -> bool {
        self.is_enemy_near().await & self.move_to_enemy().await & self.attack_enemy().await
    }

    #[instrument(skip(self), ret)]
    pub async fn is_enemy_near(&self) -> bool {
        self.enemy_near.get()
    }

    #[instrument(skip(self), ret)]
    pub async fn move_to_enemy(&self) -> bool {
        if self.moved_to_enemy.get() {
            return false;
        }
        self.moved_to_enemy.set(true);
        true
    }

    #[instrument(skip(self), ret)]
    pub async fn attack_enemy(&self) -> bool {
        if self.attacked.get() {
            return false;
        }
        self.attacked.set(true);
        true
    }

    // chill on grass
    #[instrument(skip(self), ret)]
    pub async fn chill_on_grass(&self) -> bool {
        self.standing_on_grass().await
            & self.sit_down().await
            & self.delay().await
            & self.stand_up().await
    }

    #[instrument(skip(self), ret)]
    pub async fn standing_on_grass(&self) -> bool {
        !self.sat_down.get() & self.is_on_grass.get()
    }

    #[instrument(skip(self), ret)]
    pub async fn sit_down(&self) -> bool {
        if self.sat_down.get() {
            return false;
        }
        self.sat_down.set(true);
        true
    }

    #[instrument(skip(self), ret)]
    pub async fn delay(&self) -> bool {
        if self.waited.get() {
            return false;
        }
        self.waited.set(true);
        true
    }

    #[instrument(skip(self), ret)]
    pub async fn stand_up(&self) -> bool {
        if !self.sat_down.get() {
            return false;
        }
        self.sat_down.set(true);
        true
    }
}

#[test]
fn behaviour_tree_test() {
    test_init();

    let topic = Some(broadcast::<u32>(2));

    let ctx = Context {
        topic,
        ..Default::default()
    };

    assert!(block_on(ctx.root()));
    tracing::debug!(?ctx);
}
