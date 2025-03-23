---
CPS: ??
Title: Φalanx (Phalanx) : Increasing the Computational Cost of Grinding Attacks
Category: Consensus/Security
Status: Proposed
Authors:
    - Nicolas Henin <nicolas.henin@iohk.io>
    - Raphael Toledo <raphael.toledo@iohk.io>
    - Peter Gaži <peter.gazi@iohk.io>
Proposed Solutions: []
Discussions:
    - https://github.com/cardano-foundation/CIPs/pull/1009
Created: 2025-10-03
License: Apache-2.0
---

## Table of Contents

- [**Abstract**](#abstract)
- [**Motivation: Why is this CIP necessary?**](#motivation-why-is-this-cip-necessary)
- [**Specification**](#specification)
  - [**1. The Flow**](#1-the-flow)
  - [**2. The Randomness Generation Sub-Protocol**](#2-the-randomness-generation-sub-protocol)
  - [**3. The Φ Cryptographic Primitive**](#3-the-φ-cryptographic-primitive)
  - [**4. Adversarial Cost Overhead**](#4-adversarial-cost-overhead)
  - [**5. Balancing Honest and Adversarial Computation (Performance & Scalability)**](#5-balancing-honest-and-adversarial-computation-performance--scalability)
  - [**6. Operability, Maintainability & Modularity**](#6-operability-maintainability--modularity)
  - [**7. Agda Mechanization**](#7-agda-mechanization)
- [**Rationale: How does this CIP achieve its goals?**](#rationale-how-does-this-cip-achieve-its-goals)
  - [**1. Cryptographic Primitive Selection**](#1-cryptographic-primitive-selection)
  - [**2. Performance Impacts on Consensus & Ledger Repository**](#2-performance-impacts-on-consensus--ledger-repository)
  - [**3. Maintainability**](#3-maintainability)
- [**Path to Active**](#path-to-active)
  - [**Acceptance Criteria**](#acceptance-criteria)
  - [**Implementation Plan**](#implementation-plan)
- [**Copyright**](#copyright)


## Abstract

<!-- A short (\\\~200 word) description of the proposed solution and the technical issue being addressed. \-->

Addressing the "[Ouroboros Randomness Generation Sub-Protocol – The Coin-Flipping Problem](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md#4-the-quantification-challenge)" CPS**,  Φalanx (Pronounced Phalanx)** enhances **Ouroboros Praos** to mitigate grinding attacks by **increasing the cost of leader election manipulation**. It extends **nonce generation from 1 epoch to 2**, introducing a **computationally intensive function** that remains efficient for honest participants but makes it **significantly more costly for adversaries to bias the process**.

A [**Phalanx**](https://en.wikipedia.org/wiki/Phalanx) is an **Ancient Greek military formation** where soldiers **stand in a tightly packed unit**, shielding and reinforcing one another to create a nearly impenetrable defense. This strategy made it far more difficult for enemies to break through compared to fighting individual soldiers.

![alt text](./image/image.png)

In **Φalanx Protocol**, we apply this idea cryptographically by **enhancing the VRF-based randomness generation sub-protocol** with a cryptographic primitive that is **efficient for honest participants** but **computationally expensive for adversaries** attempting to bias leader election. While it won’t eliminate grinding attacks entirely, it **significantly increases their cost**, and our work focuses on **precisely quantifying this added expense**.

Please refer to the CPS "[Ouroboros Randomness Generation Sub-Protocol – The Coin-Flipping Problem](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md#4-the-quantification-challenge)" for a detailed understanding of **randomness generation, leader election in Praos, and the coin-flipping dilemma in consensus protocols**. Moving forward, we will **dive into the core details**, assuming you have the **relevant background** to understand the proposal.

## Motivation: why is this CIP necessary?

<!-- A clear explanation that introduces the reason for a proposal, its use cases and stakeholders. If the CIP changes an established design then it must outline design issues that motivate a rework. For complex proposals, authors must write a Cardano Problem Statement (CPS) as defined in CIP-9999 and link to it as the \`Motivation\`. -->

The "[Ouroboros Randomness Generation Sub-Protocol – The Coin-Flipping Problem](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md)" CPS reveals a significant vulnerability in **Ouroboros Praos**: adversaries controlling a substantial portion of stake can execute **grinding attacks** to manipulate leader election, compromising the protocol’s fairness and security. As detailed in [CPS Section 3.2 - Entry Ticket: Acquiring Stake to Play the Lottery](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md#32-entry-ticket-acquiring-stake-to-play-the-lottery), an adversary with **20% or more of the total stake** gains an exponential advantage in influencing randomness, with attack feasibility increasing rapidly as stake grows. This critical threshold is further explored in [CPS Section 3.6 - Grinding Power Computational Feasibility](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md#36-grinding-power-computational-feasibility), which shows that grinding attacks become computationally viable for well-resourced adversaries, particularly in the "Owl Survey" scenario, where costs remain within the "Possible" range (up to $\$1$ billion USD) for grinding depths ($\rho$) between 34 and 48.

The CPS analysis in [Section 3.5 - Scenarios](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md#35-scenarios) quantifies this vulnerability across four scenarios—Ant Glance, Ant Patrol, Owl Stare, and Owl Survey—highlighting the ranges of $\rho$ where attacks are feasible. The table below summarizes these ranges, showing the intervals where grinding attacks transition from trivial to infeasible:

| **Feasibility Category**                  | **🔵 Ant Glance** | **🟠 Ant Patrol** | **🟢 Owl Stare** | **🔴 Owl Survey** |
|--------------------------------------------|-------------------|-------------------|------------------|-------------------|
| **🟢 🌱 Trivial for Any Adversary**        | $[0, 49)$         | $[0, 47)$         | $[0, 27)$        | $[0, 27)$         |
| **🟡 💰 Feasible with Standard Resources** | $[49, 59)$        | $[47, 57)$        | $[27, 34)$       | $[27, 34)$        |
| **🟠 🏭 Possible with Large-Scale Infrastructure** | $[59, 73)$ | $[57, 71)$        | $[34, 48)$       | $[34, 48)$        |
| **🔴 🚫 Borderline Infeasible**            | $[73, 87)$        | $[71, 85)$       | $[48, 62)$       | $[48, 62)$        |
| **🔴 🚫 Infeasible**                      | $[87, 256)$       | $[85, 256)$       | $[62, 256)$      | $[62, 256)$       |

This vulnerability is visually depicted in the graph below, which plots the logarithmic cost (in USD) of grinding attacks against grinding depth ($\rho$) for each scenario. The shaded feasibility layers indicate the economic thresholds where attacks become trivial, feasible, possible, borderline infeasible, or infeasible. The consistent gap of $\Delta \log_{10}(\text{Cost (USD)}) \approx 6.3$ between the least (Ant Glance) and most (Owl Survey) resource-intensive scenarios highlights how evaluation complexity ($T_{\text{eval}}$) and observation scope ($w_T$) significantly amplify attack costs :

<div align="center">
<img src="./image/grinding_depth_scenarios_cost_with_feasibility_layers_gradient.png" alt="Grinding Depth Scenarios with Feasibility Thresholds"/>
</div>

These findings indicate that, under current protocol parameters, grinding attacks are computationally viable at lower $\rho$ values for adversaries with significant resources. However, as highlighted in [CPS Section 3.2](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md#32-entry-ticket-acquiring-stake-to-play-the-lottery), executing such attacks requires a substantial upfront investment—acquiring 20% of the total stake, equivalent to over 4.36 billion ADA as of March 1, 2025—and the ability to operate covertly to avoid detection. Publicly observable grinding attempts expose adversarial stake pool operators (SPOs) to severe economic and social consequences, such as loss of trust, delegator withdrawals, or protocol-level countermeasures, which could devalue their stake and undermine their efforts. Despite these barriers, the potential for well-funded adversaries to bias randomness remains a threat to Cardano’s decentralized ethos, as it could skew block production and transaction settlement in their favor.

This CIP addresses the critical question: **Can we increase the computational cost of grinding attempts to shrink these vulnerable intervals, thereby deterring adversaries effectively?** Φalanx proposes a solution by introducing a computationally intensive mechanism that disproportionately burdens attackers while remaining manageable for honest participants. By elevating the resource threshold required for successful attacks, as analyzed in [CPS Section 3.4 - Cost of a Grinding Attack](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md#34-cost-of-a-grinding-attack), this CIP aims to shift the feasibility curve, making randomness manipulation prohibitively expensive and strengthening the protocol’s resilience against such threats.

The analysis in [Section 4 - Adversarial Cost Overhead](#4-adversarial-cost-overhead) demonstrates that Φalanx significantly increases the computational cost of grinding attacks, with a $\Delta \log_{10}(\text{Cost (USD)}) \approx 15.2$ at $\rho = 50$ between the most resource-intensive Phalanx scenario (Owl Survey $\Phi_{\text{max}}$) and the least resource-intensive Praos scenario (Ant Glance Praos). Notably, all Phalanx scenarios overlap closely in a $\log_{10}$ approximation, meaning the attack cost is largely independent of the adversary’s strategy, simplifying the assessment of attack feasibility. This allows us to focus on a unified cost model moving forward. Consequently, Phalanx shrinks the vulnerable $\rho$ intervals, as shown in the tables below, which compare the original Praos feasibility ranges with the updated Phalanx ranges for $\Phi_{\text{min}}$ and $\Phi_{\text{max}}$, including the delta improvements ($\Delta \rho$) for each Praos scenario. A positive $\Delta \rho$ indicates that Phalanx increases the cost by making attacks infeasible at lower $\rho$ values, thereby reinforcing the value of this CIP in enhancing Cardano’s security.

#### Original Praos Feasibility Ranges
| **Feasibility Category**                  | **🔵 Ant Glance** | **🟠 Ant Patrol** | **🟢 Owl Stare** | **🔴 Owl Survey** |
|--------------------------------------------|-------------------|-------------------|------------------|-------------------|
| **🟢 🌱 Trivial for Any Adversary**        | $[0, 49)$         | $[0, 47)$         | $[0, 27)$        | $[0, 27)$         |
| **🟡 💰 Feasible with Standard Resources** | $[49, 59)$        | $[47, 57)$        | $[27, 34)$       | $[27, 34)$        |
| **🟠 🏭 Possible with Large-Scale Infrastructure** | $[59, 73)$ | $[57, 71)$        | $[34, 48)$       | $[34, 48)$        |
| **🔴 🚫 Borderline Infeasible**            | $[73, 87)$        | $[71, 85)$       | $[48, 62)$       | $[48, 62)$        |
| **🔴 🚫 Infeasible**                      | $[87, 256)$       | $[85, 256)$       | $[62, 256)$      | $[62, 256)$       |

#### Phalanx with $\Phi_{\text{min}}$
| **Feasibility Category**                  | **Phalanx $\Phi_{\text{min}}$** | **🔵 Ant Glance** | **🟠 Ant Patrol** | **🟢 Owl Stare** | **🔴 Owl Survey** |
|--------------------------------------------|-------------------------------|-------------------|-------------------|------------------|-------------------|
| **🟢 🌱 Trivial for Any Adversary**        | $[0, 10)$                     | +39               | +37               | +17              | +17               |
| **🟡 💰 Feasible with Standard Resources** | $[10, 15)$                    | +44               | +42               | +19              | +19               |
| **🟠 🏭 Possible with Large-Scale Infrastructure** | $[15, 20)$            | +53               | +51               | +28              | +28               |
| **🔴 🚫 Borderline Infeasible**            | $[20, 25)$                    | +62               | +60               | +37              | +37               |
| **🔴 🚫 Infeasible**                      | $[25, 256)$                   | +62               | +60               | +37              | +37               |

#### Phalanx with $\Phi_{\text{max}}$
| **Feasibility Category**                  | **Phalanx $\Phi_{\text{max}}$** | **🔵 Ant Glance** | **🟠 Ant Patrol** | **🟢 Owl Stare** | **🔴 Owl Survey** |
|--------------------------------------------|-------------------------------|-------------------|-------------------|------------------|-------------------|
| **🟢 🌱 Trivial for Any Adversary**        | $[0, 5)$                      | +44               | +42               | +22              | +22               |
| **🟡 💰 Feasible with Standard Resources** | $[5, 10)$                     | +49               | +47               | +24              | +24               |
| **🟠 🏭 Possible with Large-Scale Infrastructure** | $[10, 15)$            | +58               | +56               | +33              | +33               |
| **🔴 🚫 Borderline Infeasible**            | $[15, 20)$                    | +67               | +65               | +42              | +42               |
| **🔴 🚫 Infeasible**                      | $[20, 256)$                   | +67               | +65               | +42              | +42               |


## Specification

<!-- The technical specification should describe the proposed improvement in sufficient technical detail. In particular, it should provide enough information that an implementation can be performed solely on the basis of the design in the CIP. This is necessary to facilitate multiple, interoperable implementations. This must include how the CIP should be versioned, if not covered under an optional Versioning main heading. If a proposal defines structure of on-chain data it must include a CDDL schema in its specification.-->

The core principle of the proposed protocol change is to **substantially escalate the computational cost of each grinding attempt for an adversary**. 

To achieve this, every honest participant is required to perform a designated computation for each block they produce over an epoch (**21,600 blocks**). Consequently, an adversary attempting a grinding attack must **recompute these operations for every single attempt**, while being **constrained by the grinding window**, which dramatically increases the resource expenditure. 

By enforcing this computational burden, we **drastically reduce the feasible number of grinding attempts** an adversary with a fixed resource budget can execute, making randomness manipulation **more expensive and significantly less practical**.
 

### 1. The Flow

In **Φalanx** , the randomness generation and leader election flows are modified as follows:

![alt text](./image/image-1.png)

1. The **stake distribution stabilization phase** is shifted **back by one epoch :** The **active** **stake distribution** *SDe* used for leader election is now derived from the **end of $epoch_\text{e-3}$** instead of **$epoch_\text{e-2}$**  as in the original Praos protocol.  
2. The **honest contribution inclusion phase**, which originally resulted in a **ηₑ candidate**, is also **shifted back by one epoch**, aligning with the adjusted **stake distribution stabilization**. This value is now referred to as the **pre-ηₑ candidate**, signifying its role as an **intermediate randomness nonce** in the sub-protocol.  
3. The **ηₑ (randomness eta nonce)** undergoes an **additional sequence of incremental hashing** using a **new deterministic** **cryptographic primitive Φ (Phi)**, applied over a duration equivalent to a full epoch.

### 2. The Randomness Generation Sub-Protocol 

The Randomness Generation sub-protocol operates with two parallel streams: $`\eta^\text{stream}`$ and $`\phi^\text{stream}`$, which synchronize at the conclusion of the **Include Honest Contribution** Phase (akka Phase 2).  

##### **The $`\eta^\text{stream}`$ Definition** 
   - Exist in Praos already and will be kept in Phalanx  
   - **Sampling Rate**  : For every block produced within the blockchain tree, a unique $`\eta^\text{stream}`$ is appended in the block header :

```math
   \eta^{\text{stream}}_{t+1} =
   \begin{cases}
   \text{ProtocolParameter}_\text{extraEntropy} & \text{when } t = 0, \\
   \eta^{\text{stream}}_{t} ⭒ VRF^\text{Output}_\text{t+1} & \text{when BlockProduced}(t) \\
   \eta^{\text{stream}}_{t}  & \text{otherwise.}
   \end{cases}
   
```
```math 
\text{BlockProduced}(t) = 
\begin{cases} 
true & \text{if a block is produced at time } t, \\
false & \text{otherwise.}
\end{cases}
```

| **where** ||
|---------------|-----------------|
| $`\text{ProtocolParameter}_\text{extraEntropy} `$ | The evolving nonce is initialized using the extraEntropy field defined in the protocol parameters.|
| $` VRF^\text{Output}_\text{i} `$ | The **VRF output** generated by the $` \text{slot}_\text{i} `$ Leader and included in the block header |
| $a⭒b$    | The concatenation of $a$ and $b$ , followed by a BLAKE2b-256 hash computation.

##### **The $`\text{pre-}\eta`$ Synchronizations**  

- To generate $`\eta_\text{e}`$ for epoch $`e`$, the stream $`\phi^\text{stream}`$ is reset with the value of $`\eta^\text{stream}`$ at the end of Phase 2 in $`\text{epoch}_{e-2}`$. The value of $`\eta^\text{stream}`$ at that moment is referred to as $`\text{pre-}\eta_e`$.
 
##### **The $`\phi^\text{stream}`$ Definition**  

   - It is reset at the $`\text{pre-}\eta`$ Synchronizations
   - For every block produced within the blockchain tree, a unique $`\phi^\text{evolving}`$ is appended in the block header :

```math
   \phi^{\text{stream}}_{t+1} =
   \begin{cases}
   \eta^\text{stream}_{t} & \text{when } t = \text{pre-}\eta\text{ synchronization}, \\
   \Phi(\phi^{\text{stream}}_{t})  & \text{when t = Active Slot} \\
   \phi^{\text{stream}}_{t}  & \text{otherwise.}
   \end{cases}
```

##### **The $`\eta`$** Generations
   - This is the final nonce $`\eta_\text{e}`$ used to determine participant eligibility during epoch $`e`$.  
   - It originates from $`\phi^{\text{stream}}_{t}`$ at $`\text{pre-}\eta_\text{e+1}`$ Synchronization  XOR with $`\eta^\text{stream}_t`$ $`\text{when } t = \text{end of epoch}_\text{e-3}`$   

```math
\eta_\text{e} = \eta^\text{stream}_{epoch_\text{e-3}} ⭒ \phi^\text{stream}_t , \quad \text{when } t = \text{pre-}\eta_\text{e+1}\text{ synchronization } 
```
**N.B** : $`\text{pre-}\eta_\text{e+1}`$ synchronization occurs $`\text{when } t = \text{end of phase 2 at epoch}_\text{e-1}`$

### 3. The Φ Cryptographic Primitive

The Φ cryptographic primitive is a critical component of the Φalanx protocol, designed to increase the computational cost of grinding attacks while remaining efficient for honest participants. To achieve this, Φ must adhere to a set of well-defined properties that ensure its security, efficiency, and practical usability within the Cardano ecosystem. These properties are outlined in the table below :

| **Property**              | **Description**                                                                                                   |
|---------------------------|-------------------------------------------------------------------------------------------------------------------|
| **Functionality**         | Must be a well-defined mathematical function, ensuring a unique output for each given input (unlike proof-of-work, which allows multiple valid outputs). |
| **Determinism**           | Must be fully deterministic, with the output entirely determined by the input, eliminating non-deterministic variations. |
| **Efficient Verification**| Must allow for fast and lightweight verification, enabling rapid validation of outputs with minimal computational overhead. |
| **Compact Representation**| Input and output sizes should be small enough to fit within a block, optimizing on-chain storage efficiency. Further reductions are desirable where feasible. |
| **Lower Bound on Computation** | Computational cost of evaluation should be well-characterized and predictable, with a lower bound that is difficult to surpass, ensuring adversaries cannot gain an unfair efficiency advantage. |
| **Ease of Implementation & Maintenance** | Should be simple to implement and maintain, ensuring long-term usability and minimizing technical debt. |
| **Adaptive Security**     | Function and its parameters should be easily reconfigurable to accommodate evolving threats, such as advances in computational power or new cryptographic attacks. |


### 4. Adversarial Cost Overhead

We have detailed in the CPS [Section 3.4.1 - Formula](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md#341-formula) the computational cost of a grinding attack, culminating in the estimated formula using Cardano mainnet parameters in [Section 3.4.2 - Estimated Formula Using Mainnet Cardano Parameters](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md#342-estimated-formula-using-mainnet-cardano-parameters):

```math
 N_{\text{CPU}} > \left \lceil 5 \times 10^{-10} \times 2^{\rho-1} + \frac{5 \times 10^{-14} \times 2^{\rho-1}}{\rho} \cdot w_T + \frac{5 \times 10^{-2} \times 2^{\rho-1}}{\rho} \cdot T_{\text{eval}} \right \rceil 
```

In Phalanx, we introduce an additional computational cost, $T_\Phi$, for each grinding attempt, which arises from the new $\Phi$ cryptographic primitive applied across all blocks in an epoch. This cost is defined as:

```math
T_\Phi = \frac{10k \cdot T_\phi}{f} 
```

Where:
- $10k/f = 432,000$ slots (number of slots in an epoch, with $k = 2,160$, $f = 0.05$),
- $T_\phi$: Time to compute an iteration $\phi$ of $\Phi$ function per block.

For a grinding attack with grinding depth $\rho$, the adversary must evaluate $2^\rho$ possible nonces, each requiring the recomputation of the $\Phi$ function across the entire epoch. Thus, the total additional cost for the attack is $2^\rho \cdot T_\Phi$. Incorporating this into the formula, the updated computational requirement becomes:

```math
N_{\text{CPU}} > \left \lceil 2^\rho \cdot T_\Phi + 5 \times 10^{-10} \times 2^{\rho-1} + \frac{5 \times 10^{-14} \times 2^{\rho-1}}{\rho} \cdot w_T + \frac{5 \times 10^{-2} \times 2^{\rho-1}}{\rho} \cdot T_{\text{eval}} \right \rceil
```

To evaluate the impact of Phalanx on grinding attack feasibility, we revisit the four scenarios defined in [CPS Section 3.5 - Scenarios](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md#35-scenarios)—Ant Glance, Ant Patrol, Owl Stare, and Owl Survey—and extend them to include Phalanx-enhanced versions. These scenarios use an animal-inspired metaphor to reflect evaluation complexity ($T_{\text{eval}}$) and observation scope ($w_T$), providing a basis for comparing the computational cost under Praos. We incorporate the additional computational cost $T_\Phi$, with $T_\phi$ values of $100  \text{ms}$ ($0.1  \text{s}$) for $\Phi_{\text{min}}$ (where $T_\Phi = 4.32 \times 10^4  \text{s}$) and $1  \text{s}$ for $\Phi_{\text{max}}$ (where $T_\Phi = 4.32 \times 10^5  \text{s}$).

| **Scenario**            | **$T_{\text{eval}}$ (Complexity)** | **$w_T$ (Scope)**       | **Description**                                                                 |
|--------------------------|------------------------------------|-------------------------|---------------------------------------------------------------------------------|
| **Ant Glance Praos**     | $0 \text{s}$                           | $1  \text{h}$  | An ant quickly glancing at a small spot, representing simple evaluation (low $T_{\text{eval}}$) with basic effort and a narrow observation scope (small $w_T$). |
| **Ant Glance $\Phi_{\text{min}}$** | $0 \text{s}$                           | $1  \text{h}$  | An ant glancing with Phalanx’s minimal $\Phi$ cost, adding moderate effort due to $T_\Phi = 4.32 \times 10^4  \text{s}$. |
| **Ant Glance $\Phi_{\text{max}}$** | $0 \text{s}$                           | $1  \text{h}$  | An ant glancing with Phalanx’s maximal $\Phi$ cost, significantly increasing effort due to $T_\Phi = 4.32 \times 10^5  \text{s}$. |
| **Ant Patrol Praos**     | $0 \text{s}$                           | $5  \text{d}$  | An ant patrolling a wide area over time with simple instincts, representing simple evaluation (low $T_{\text{eval}}$) with basic effort and a broad observation scope (large $w_T$). |
| **Ant Patrol $\Phi_{\text{min}}$** | $0 \text{s}$                           | $5  \text{d}$  | An ant patrolling with Phalanx’s minimal $\Phi$ cost, adding moderate effort across the wide scope. |
| **Ant Patrol $\Phi_{\text{max}}$** | $0 \text{s}$                           | $5  \text{d}$  | An ant patrolling with Phalanx’s maximal $\Phi$ cost, significantly increasing effort across the wide scope. |
| **Owl Stare Praos**      | $1 \text{s}$                          | $1  \text{h}$  | An owl staring intently at a small area with keen focus, representing complex evaluation (high $T_{\text{eval}}$) with advanced effort and a narrow observation scope (small $w_T$). |
| **Owl Stare $\Phi_{\text{min}}$**  | $1 \text{s}$                          | $1 \text{h}$  | An owl staring with Phalanx’s minimal $\Phi$ cost, adding moderate effort to its complex evaluation. |
| **Owl Stare $\Phi_{\text{max}}$**  | $1 \text{s}$                          | $1 \text{h}$  | An owl staring with Phalanx’s maximal $\Phi$ cost, significantly increasing effort in its complex evaluation. |
| **Owl Survey Praos**     | $1 \text{s}$                          | $5  \text{d}$ | An owl surveying a wide range with strategic awareness, representing complex evaluation (high $T_{\text{eval}}$) with advanced effort and a broad observation scope (large $w_T$). |
| **Owl Survey $\Phi_{\text{min}}$** | $1 \text{s}$                          | $5 \text{d}$ | An owl surveying with Phalanx’s minimal $\Phi$ cost, adding moderate effort across the broad scope. |
| **Owl Survey $\Phi_{\text{max}}$** | $1 \text{s}$                          | $5 \text{d}$ | An owl surveying with Phalanx’s maximal $\Phi$ cost, significantly increasing effort across the broad scope. |

The $N_{\text{CPU}}$ formulas are derived by substituting the respective $w_T$ and $T_{\text{eval}}$ values from each scenario into the base expression from Section 4:

```math
N_{\text{CPU}} > \left \lceil 5 \times 10^{-10} \times 2^{\rho-1} + \frac{5 \times 10^{-14} \times 2^{\rho-1}}{\rho} \cdot w_T + \frac{5 \times 10^{-2} \times 2^{\rho-1}}{\rho} \cdot T_{\text{eval}} + 4.32 \times 10^4 \times 2^\rho \right \rceil \quad \text{for } \Phi_{\text{min}}
```

```math
N_{\text{CPU}} > \left \lceil 5 \times 10^{-10} \times 2^{\rho-1} + \frac{5 \times 10^{-14} \times 2^{\rho-1}}{\rho} \cdot w_T + \frac{5 \times 10^{-2} \times 2^{\rho-1}}{\rho} \cdot T_{\text{eval}} + 4.32 \times 10^5 \times 2^\rho \right \rceil \quad \text{for } \Phi_{\text{max}}
```

| **Scenario**            | **$N_{\text{CPU}}$ Formula**                                                                                     |
|--------------------------|-----------------------------------------------------------------------------------------------------------------|
| **Ant Glance Praos**     | $5 \times 10^{-10} \times 2^{\rho-1} + 1.8 \times 10^{-11} \times 2^{\rho-1}$                                  |
| **Ant Glance $\Phi_{\text{min}}$** | $5 \times 10^{-10} \times 2^{\rho-1} + 1.8 \times 10^{-11} \times 2^{\rho-1} + 4.32 \times 10^4 \times 2^\rho$  |
| **Ant Glance $\Phi_{\text{max}}$** | $5 \times 10^{-10} \times 2^{\rho-1} + 1.8 \times 10^{-11} \times 2^{\rho-1} + 4.32 \times 10^5 \times 2^\rho$  |
| **Ant Patrol Praos**     | $5 \times 10^{-10} \times 2^{\rho-1} + 2.16 \times 10^{-9} \times 2^{\rho-1}$                                  |
| **Ant Patrol $\Phi_{\text{min}}$** | $5 \times 10^{-10} \times 2^{\rho-1} + 2.16 \times 10^{-9} \times 2^{\rho-1} + 4.32 \times 10^4 \times 2^\rho$  |
| **Ant Patrol $\Phi_{\text{max}}$** | $5 \times 10^{-10} \times 2^{\rho-1} + 2.16 \times 10^{-9} \times 2^{\rho-1} + 4.32 \times 10^5 \times 2^\rho$  |
| **Owl Stare Praos**      | $5 \times 10^{-10} \times 2^{\rho-1} + 1.8 \times 10^{-11} \times 2^{\rho-1} + 5 \times 10^{-2} \times \frac{2^{\rho-1}}{\rho}$ |
| **Owl Stare $\Phi_{\text{min}}$**  | $5 \times 10^{-10} \times 2^{\rho-1} + 1.8 \times 10^{-11} \times 2^{\rho-1} + 5 \times 10^{-2} \times \frac{2^{\rho-1}}{\rho} + 4.32 \times 10^4 \times 2^\rho$ |
| **Owl Stare $\Phi_{\text{max}}$**  | $5 \times 10^{-10} \times 2^{\rho-1} + 1.8 \times 10^{-11} \times 2^{\rho-1} + 5 \times 10^{-2} \times \frac{2^{\rho-1}}{\rho} + 4.32 \times 10^5 \times 2^\rho$ |
| **Owl Survey Praos**     | $5 \times 10^{-10} \times 2^{\rho-1} + 2.16 \times 10^{-9} \times 2^{\rho-1} + 5 \times 10^{-2} \times \frac{2^{\rho-1}}{\rho}$ |
| **Owl Survey $\Phi_{\text{min}}$** | $5 \times 10^{-10} \times 2^{\rho-1} + 2.16 \times 10^{-9} \times 2^{\rho-1} + 5 \times 10^{-2} \times \frac{2^{\rho-1}}{\rho} + 4.32 \times 10^4 \times 2^\rho$ |
| **Owl Survey $\Phi_{\text{max}}$** | $5 \times 10^{-10} \times 2^{\rho-1} + 2.16 \times 10^{-9} \times 2^{\rho-1} + 5 \times 10^{-2} \times \frac{2^{\rho-1}}{\rho} + 4.32 \times 10^5 \times 2^\rho$ |


The graph below illustrates the logarithmic cost (in USD) of grinding attacks across Praos and Phalanx scenarios as a function of grinding depth ($\rho$). Solid lines represent the original Praos scenarios, dashed lines represent Phalanx with $\Phi_{\text{min}}$, and dotted lines represent Phalanx with $\Phi_{\text{max}}$. The shaded feasibility layers indicate economic thresholds where attacks become trivial, feasible, possible, borderline infeasible, or infeasible, as defined in [CPS Section 3.6 - Grinding Power Computational Feasibility](https://github.com/input-output-hk/ouroboros-anti-grinding-design/blob/main/CPS/Readme.md#36-grinding-power-computational-feasibility). The $\Delta \log_{10}(\text{Cost (USD)}) \approx 15.2$ at $\rho = 50$ between the most resource-intensive Phalanx scenario (Owl Survey $\Phi_{\text{max}}$) and the least resource-intensive Praos scenario (Ant Glance Praos) highlights the significant cost increase introduced by Phalanx.

<div align="center">
<img src="./image/grinding_depth_scenarios_cost_praos_vs_phalanx.png" alt="Cost of Grinding Attacks: Praos vs Phalanx Scenarios"/>
</div>

✏️ **Note**: The code to generate this graph is available at ➡️ [this link](./graph/scenario_cost_praos_vs_phalanx.py).

### Interpretation of the Graph

A key observation from the graph is that all Phalanx scenarios—whether $\Phi_{\text{min}}$ or $\Phi_{\text{max}}$—overlap closely in a $\log_{10}$ approximation across the range of $\rho$ values. This overlap is a significant advantage because it indicates that the computational cost of a grinding attack under Phalanx is largely independent of the adversary’s strategy (i.e., the choice of scenario, which varies by $T_{\text{eval}}$ and $w_T$). In other words, regardless of whether an adversary opts for a simple strategy like Ant Glance or a complex one like Owl Survey, the $\log_{10}$ cost of the attack remains effectively the same under Phalanx. This uniformity simplifies our reasoning about adversarial behavior: we no longer need to analyze distinct scenarios to assess the feasibility of grinding attacks. Moving forward, we can focus on a single cost model for Phalanx, treating the attack cost as a function of $\rho$ and the $\Phi$ parameter ($T_\phi$), without differentiating between strategic variations.


### Impact on Feasibility Categories

This simplification allows us to revisit and improve the feasibility category table presented in the Motivation section, which originally detailed the $\rho$ ranges for each Praos scenario. With Phalanx, the overlap of scenarios enables us to consolidate the analysis into a single set of feasibility ranges based on the $\Phi_{\text{min}}$ and $\Phi_{\text{max}}$ configurations. The tables below first present the original Praos feasibility ranges, followed by the updated categories for Phalanx, reflecting the increased computational cost and the unified cost model. The Phalanx tables include the delta improvements ($\Delta \rho$) for each Praos scenario, showing the reduction in the upper bound of each feasibility category compared to the original Praos ranges. A positive $\Delta \rho$ indicates that Phalanx increases the cost by making attacks infeasible at lower $\rho$ values.

#### Original Praos Feasibility Ranges
| **Feasibility Category**                  | **🔵 Ant Glance** | **🟠 Ant Patrol** | **🟢 Owl Stare** | **🔴 Owl Survey** |
|--------------------------------------------|-------------------|-------------------|------------------|-------------------|
| **🟢 🌱 Trivial for Any Adversary**        | $[0, 49)$         | $[0, 47)$         | $[0, 27)$        | $[0, 27)$         |
| **🟡 💰 Feasible with Standard Resources** | $[49, 59)$        | $[47, 57)$        | $[27, 34)$       | $[27, 34)$        |
| **🟠 🏭 Possible with Large-Scale Infrastructure** | $[59, 73)$ | $[57, 71)$        | $[34, 48)$       | $[34, 48)$        |
| **🔴 🚫 Borderline Infeasible**            | $[73, 87)$        | $[71, 85)$       | $[48, 62)$       | $[48, 62)$        |
| **🔴 🚫 Infeasible**                      | $[87, 256)$       | $[85, 256)$       | $[62, 256)$      | $[62, 256)$       |

#### Phalanx with $\Phi_{\text{min}}$
| **Feasibility Category**                  | **Phalanx $\Phi_{\text{min}}$** | **🔵 Ant Glance** | **🟠 Ant Patrol** | **🟢 Owl Stare** | **🔴 Owl Survey** |
|--------------------------------------------|-------------------------------|-------------------|-------------------|------------------|-------------------|
| **🟢 🌱 Trivial for Any Adversary**        | $[0, 10)$                     | +39               | +37               | +17              | +17               |
| **🟡 💰 Feasible with Standard Resources** | $[10, 15)$                    | +44               | +42               | +19              | +19               |
| **🟠 🏭 Possible with Large-Scale Infrastructure** | $[15, 20)$            | +53               | +51               | +28              | +28               |
| **🔴 🚫 Borderline Infeasible**            | $[20, 25)$                    | +62               | +60               | +37              | +37               |
| **🔴 🚫 Infeasible**                      | $[25, 256)$                   | +62               | +60               | +37              | +37               |

#### Phalanx with $\Phi_{\text{max}}$
| **Feasibility Category**                  | **Phalanx $\Phi_{\text{max}}$** | **🔵 Ant Glance** | **🟠 Ant Patrol** | **🟢 Owl Stare** | **🔴 Owl Survey** |
|--------------------------------------------|-------------------------------|-------------------|-------------------|------------------|-------------------|
| **🟢 🌱 Trivial for Any Adversary**        | $[0, 5)$                      | +44               | +42               | +22              | +22               |
| **🟡 💰 Feasible with Standard Resources** | $[5, 10)$                     | +49               | +47               | +24              | +24               |
| **🟠 🏭 Possible with Large-Scale Infrastructure** | $[10, 15)$            | +58               | +56               | +33              | +33               |
| **🔴 🚫 Borderline Infeasible**            | $[15, 20)$                    | +67               | +65               | +42              | +42               |
| **🔴 🚫 Infeasible**                      | $[20, 256)$                   | +67               | +65               | +42              | +42               |

</br>

<details>
  <summary>📌📌 <i>Delta Improvement Calculations</i> – <b>Expand to view the content</b>.</summary>
  <p>

- **Trivial**:
  - $\Phi_{\text{min}}$: Ant Glance: $49 - 10 = 39$, Ant Patrol: $47 - 10 = 37$, Owl Stare: $27 - 10 = 17$, Owl Survey: $27 - 10 = 17$
  - $\Phi_{\text{max}}$: Ant Glance: $49 - 5 = 44$, Ant Patrol: $47 - 5 = 42$, Owl Stare: $27 - 5 = 22$, Owl Survey: $27 - 5 = 22$
- **Feasible**:
  - $\Phi_{\text{min}}$: Ant Glance: $59 - 15 = 44$, Ant Patrol: $57 - 15 = 42$, Owl Stare: $34 - 15 = 19$, Owl Survey: $34 - 15 = 19$
  - $\Phi_{\text{max}}$: Ant Glance: $59 - 10 = 49$, Ant Patrol: $57 - 10 = 47$, Owl Stare: $34 - 10 = 24$, Owl Survey: $34 - 10 = 24$
- **Possible**:
  - $\Phi_{\text{min}}$: Ant Glance: $73 - 20 = 53$, Ant Patrol: $71 - 20 = 51$, Owl Stare: $48 - 20 = 28$, Owl Survey: $48 - 20 = 28$
  - $\Phi_{\text{max}}$: Ant Glance: $73 - 15 = 58$, Ant Patrol: $71 - 15 = 56$, Owl Stare: $48 - 15 = 33$, Owl Survey: $48 - 15 = 33$
- **Borderline Infeasible**:
  - $\Phi_{\text{min}}$: Ant Glance: $87 - 25 = 62$, Ant Patrol: $85 - 25 = 60$, Owl Stare: $62 - 25 = 37$, Owl Survey: $62 - 25 = 37$
  - $\Phi_{\text{max}}$: Ant Glance: $87 - 20 = 67$, Ant Patrol: $85 - 20 = 65$, Owl Stare: $62 - 20 = 42$, Owl Survey: $62 - 20 = 42$
- **Infeasible**:
  - The $\Delta \rho$ for the "Infeasible" category is the same as "Borderline Infeasible" since it reflects the shift in the upper bound of the previous category.

  </p>
</details>
</br>

This updated table demonstrates a significant improvement over the Praos scenarios. For $\Phi_{\text{min}}$, the "Trivial" range shrinks to $\rho < 10$ (a reduction of up to 39 for Ant Glance Praos), and the "Possible" range is limited to $\rho < 20$ (a reduction of up to 53). For $\Phi_{\text{max}}$, the effect is even more pronounced, with the "Trivial" range reduced to $\rho < 5$ (a reduction of up to 44) and the "Possible" range to $\rho < 15$ (a reduction of up to 58). These substantial $\Delta \rho$ values indicate that Phalanx significantly raises the bar for grinding attacks, pushing the feasibility thresholds to much lower $\rho$ values across all scenarios. This makes such attacks economically and computationally prohibitive for adversaries, even those with significant resources, thereby enhancing the security of the Ouroboros Praos protocol.

### 5. Balancing Honest and Adversarial Computation (Performance & Scalability)


Work in Progress in [google doc](https://docs.google.com/document/d/13TZF2jYLoKPjs6Aa9tLA4t9TtxqhBB7qMIZCy9SWKR4/edit?tab=t.0)


### 6. Operability, Maintainability & Modularity

Work in Progress in [google doc](https://docs.google.com/document/d/13TZF2jYLoKPjs6Aa9tLA4t9TtxqhBB7qMIZCy9SWKR4/edit?tab=t.0)


### 7. Agda Mechanization

Todo  

## Rationale: how does this CIP achieve its goals?
<!-- The rationale fleshes out the specification by describing what motivated the design and what led to particular design decisions. It should describe alternate designs considered and related work. The rationale should provide evidence of consensus within the community and discuss significant objections or concerns raised during the discussion.

It must also explain how the proposal affects the backward compatibility of existing solutions when applicable. If the proposal responds to a CPS, the 'Rationale' section should explain how it addresses the CPS, and answer any questions that the CPS poses for potential solutions.
-->

### 1. Cryptographic Primitive Selection

Work in Progress in [google doc](https://docs.google.com/document/d/13TZF2jYLoKPjs6Aa9tLA4t9TtxqhBB7qMIZCy9SWKR4/edit?tab=t.0)

[Consolidation of this google doc - Anti-Grinding: the Cryptography](https://docs.google.com/document/d/1zXMdoIlwnVSYjz46jxXuNPIWi-xPXxUjltF-8g7TJTc/edit?tab=t.0#heading=h.wefcmsmvzoy5)

### 2. Performance Impacts on Consensus & Ledger Repository

Todo : Simulation of Phalanx for Honest Participant for refining $\Phi_{\text{min}}$ and $\Phi_{\text{max}}$ defined in Specification.

### 3. Maintainability

Todo 

## Path to Active

### Acceptance Criteria
<!-- Describes what are the acceptance criteria whereby a proposal becomes 'Active' -->

Todo

### Implementation Plan
<!-- A plan to meet those criteria or `N/A` if an implementation plan is not applicable. -->
Todo
<!-- OPTIONAL SECTIONS: see CIP-0001 > Document > Structure table -->

## Copyright
<!-- The CIP must be explicitly licensed under acceptable copyright terms.  Uncomment the one you wish to use (delete the other one) and ensure it matches the License field in the header: -->
Todo
<!-- This CIP is licensed under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/legalcode). -->
<!-- This CIP is licensed under [Apache-2.0](http://www.apache.org/licenses/LICENSE-2.0). -->