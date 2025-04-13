# Future Behavior States

A lightweight Rust library to seamlessly compose and execute **behavior trees** and **state machines** on top of Rust's native `async` infrastructure.

## Overview

This crate provides minimal abstractions that enable **interoperability** between state machines (SMs) and behavior trees (BTs). Instead of reinventing an execution engine, this crate leverages Rust’s built-in `async` and `await` mechanisms, directly modeling execution flow with idiomatic futures.

This library is not a framework—it’s a thin layer of glue between two paradigms of decision-making logic that traditionally live in different domains but **can coexist and complement each other**.

## Why This Exists

### Behavior Trees

Behavior trees are commonly used in **game AI** and **robotics**. They are highly structured, composable, and good for representing **goal-directed sequences** with clear success/failure outcomes.

### State Machines

State machines are ubiquitous in **embedded systems**, **industrial automation**, **network protocols**, and also have theoretical roots in **Turing machines**. They are excellent for **reactive** systems and managing **event-driven transitions**.

### The Dilemma

-   **Behavior Trees** shine for complex logic flow, but are awkward for reactive event handling.
    
-   **State Machines** are intuitive and reactive but become hard to manage when many transitions and interactions emerge.

### So… Why Choose?

This library allows you to **compose both behavior trees and state machines together** in a unified hierarchy. Behaviors can embed state machines and vice versa.


##  Key Concepts

### 1. Behavior

A behavior is a future that resolves to a boolean:

```rust
async  fn  example_sequence_behavior() ->  bool { check_one().await && check_two().await && act().await }
```
Behaviors:
-   Represent **tasks or checks**.    
-   Return `true` (success) or `false` (failure).    
-   Are statically typed (`impl Future<Output = bool>`).    
-   Are **structured at compile time**, making them lightweight.

### 2. State

A state contains an `async` function that returns the next `State`:

```rust
async  fn  example_state() -> State {
    select!(
            // transitions when check behavior returns true
            state_one().when(|| check_one()),
            state_two().when(|| check_two()),
            // internal behavior, transitions to SUCCESS when task returns true
            success().when(|| task()),
    )
}
```
States:
-   Use **dynamic dispatch** to enable cycles and flexible transitions.
-   Returns a `State` enum of`Success`, `Failure`, or trait object `Running(future::BoxedLocal<State>)`.
-   Are reactive, handling multiple simultaneous transitions.


### 3. Interchangeability

| From       | To         | Mechanism |
|------------|------------|-----------|
| State      | Behavior   | Execute state until terminal transition returns success/failure (e.g., `state.eval()`).|
| Behavior   | State Transition     | Treat behavior as a decision rule (e.g., `next_state.when(|| check_behavior())`). |
| Behavior   | State Internal Action | Use behavior result as transition to Success/Failure, or discard result and repeat for non terminating states. |

### 4. Hierarchical Composition

-   Behavior trees are inherently hierarchical.
-   State machines achieve hierarchy by nesting state machines as a state's internal action.
-   A state can embed a behavior tree as its internal action.
-   A behavior tree can embed a state as a child behavior, executing it until a terminal state is reached.

This enables seamless coexistence and interchangeability between both paradigms at any level of the hierarchy.

## Example Usage

- For behavior tree examples, refer to: [Behavior Tree](tests/behavior_tree.rs)
- For state machine examples, refer to: [State Machine](tests/state_machine.rs)

