
- CPS: 18
- Title: `Ouroboros Randomness Manipulation`
- Category: Consensus/Security
- Status: Proposed
- Authors:
    - `Nicolas Henin <nicolas.henin@iohk.io>`
- Implementors: []
- Discussions:
    - `https://github.com/cardano-foundation/CIPs/pull/?`
- Created: `2024-10-03`
- License: `CC-BY-4.0`

## Abstract

<!-- A short (\~200 word) description of the target goals and the technical obstacles to those goals. -->

The security and fairness of Ouroboros, Cardano’s consensus protocol, hinge on the integrity of its Verifiable Random Function (VRF)-based leader election process. This document aims to demonstrate that the randomness in this process can be **manipulated** and the risks associated to it, albeit at the cost of significant computational effort and adversarial stake.  

To uphold Cardano’s decentralized ethos, the community must proactively mitigate these risks and **reduce the feasibility of biasing strategies**. Addressing this challenge requires answering key questions:  

- **Is Cardano currently being manipulated?**  
  Strengthening **detection mechanisms**, such as **self-mixing analysis** and **forking manipulation detection**, can help monitor potential exploits and assess ongoing threats.  

- **Are we sufficiently disincentivizing randomness manipulation?**  
  Promoting **stake operator diversity** and enhancing incentives for decentralization will make manipulation economically unviable, fostering a more **resilient and distributed** stake pool ecosystem.  

- **How vulnerable is Cardano to these attacks, and what are the potential consequences?**  
  Refining **risk quantification** will provide a clearer understanding of vulnerabilities, attack feasibility, and potential security gaps within the protocol.  

Beyond **detection, assessment, and quantification**, **protocol-level adjustments** must be explored to directly **curb manipulation opportunities** and strengthen incentives for honest participation.  

Finally, it is crucial to recognize that **adversarial capabilities continuously evolve**, making this an **ongoing challenge** that requires sustained research, adaptation, and community-driven innovation.  


## Problem

<!-- A more elaborate description of the problem and its context. This section should explain what motivates the writing of the CPS document. -->

- <a href="#1-preliminaries">1. Preliminaries</a>
  - <a href="#11-fundamental-properties">1.1 Fundamental Properties</a>
    - <a href="#111-transaction-ledger-properties">1.1.1 Transaction Ledger Properties</a>
      - <a href="#1111-persistence-with-the-security-parameter-k">1.1.1.1 Persistence with the security parameter $`k`$</a>
      - <a href="#1112-liveness-with-the-transaction-confirmation-time-parameter-u">1.1.1.2 Liveness with the transaction confirmation time parameter $`u`$</a>
    - <a href="#112-chains-properties">1.1.2 Chain Properties</a>
      - <a href="#1121-common-prefix-cp">1.1.2.1 Common Prefix (CP)</a>
      - <a href="#1122-honest-bounded-chain-growth-hcg">1.1.2.2 Honest-Bounded Chain Growth (HCG)</a>
      - <a href="#1123-existential-chain-quality-cq">1.1.2.3 Existential Chain Quality (∃CQ)</a>
      - <a href="#1124-chain-growth-cg">1.1.2.4 Chain Growth (CG)</a>
  - <a href="#12-leader-election-in-praos">1.2 Leader Election in Praos</a>
    - <a href="#121-oblivious-leader-selection">1.2.1 Oblivious Leader Selection</a>
    - <a href="#122-application-of-verifiable-random-function-vrf">1.2.2 Application of Verifiable Random Function (VRF)</a>
    - <a href="#123-eligibility-check-input-variables">1.2.3 Eligibility Check Input Variables</a>
    - <a href="#124-epoch-structure">1.2.4 Epoch Structure</a>
    - <a href="#125-epoch-phases-length">1.2.5 Epoch & Phases Length</a>
  - <a href="#13-forks-rollbacks-finality-and-settlement-times">1.3 Forks, Rollbacks, Finality, and Settlement Times</a>

- <a href="#2-randomness-manipulation-objectives">2. Randomness Manipulation Objectives</a>
  - <a href="#21-exposure">2.1 Exposure</a>
  - <a href="#22-slot-leader-distribution-selection">2.2 Slot Leader Distribution Selection</a>

- <a href="#3-non-exhaustive-manipulation-strategy-list">3. Non-Exhaustive Manipulation Strategy List</a>
  - <a href="#31-system-model">3.1 System Model</a>
  - <a href="#32-self-mixing-strategy">3.2 Self-Mixing Strategy</a>
  - <a href="#33-forking-strategies">3.3 Forking Strategies</a>

- <a href="#4-the-quantification-challenge">4. The Quantification Challenge</a>
##


To fully grasp the context and accurately assess the level of vulnerability, it is crucial to **clearly define how the Praos protocol handles randomness** and eliminate any ambiguity in its implementation. This precise understanding will then serve as a foundation for **identifying and defining these potential attack vectors**. We will refer to these types of attacks as Grinding Attacks throughout this document.

# 1. Preliminaries

This section introduces the pertinent parts of the Cardano proof- of-stake consensus protocol. We focus on randomness generation and leader selection and omit irrelevant protocol details.

## 1.1 Fundamental Properties

A protocol implements a robust transaction ledger if it maintains the ledger as a sequence of blocks, where each block is associated with a specific slot. Each slot can contain at most one ledger block, and this strict association ensures a well-defined and immutable ordering of transactions within the ledger. This structure is critical for preventing manipulation of the transaction order, which is a key vector for grinding attacks. 

For the ledger to be resistant to such attacks, the protocol must satisfy the following two critical properties (Persistence & Liveness), which ensure that blocks and transactions are securely committed and cannot be easily manipulated by adversaries. Persistence and liveness, can be derived to fundamental **chain properties** which are *used to explain how and why the leader election mechanism has been designed in this manner*. 

| **Chain Property**                      | **Description**                                                                                                                    |
|-----------------------------------------|------------------------------------------------------------------------------------------------------------------------------|
| **Common Prefix (CP)**                  | Ensures consistency across honest parties by requiring that chains adopted by different honest nodes share a common prefix.   |
| **Honest-Bounded Chain Growth (HCG)**   | Governs the rate at which the blockchain grows, ensuring that the chain grows at a rate proportional to time.     |
| **Existential Chain Quality (∃CQ)**     | Guarantees that at least one honestly-generated block appears in any given portion of the chain, ensuring honest contributions. |
| **Chain Growth (CG)**                   | Provides a more general notion of growth by combining **HCG** and **∃CQ**, ensuring both the quality and rate of chain expansion. |

### 1.1.1 Transaction Ledger Properties 
#### 1.1.1.1 Persistence with the **security parameter $` \text{k} \in \mathbb{N} `$**
 
Once a node of the system proclaims a certain transaction *tx* in the stable part of its ledger, the remaining nodes, if queried, will either report *tx* in the same position of that ledger or report a stable ledger which is a prefix of that ledger. Here the notion of stability is a predicate that is parameterized by a **security parameter $` \text{k} `$**. Specifically, a transaction is declared **stable** if and only if it is in a block that is more than $` \text{k} `$ blocks deep in the ledger.

#### 1.1.1.2 Liveness with the **transaction confirmation time parameter $` u \in \mathbb{N} `$** 

If all honest nodes in the system attempt to include a certain transaction then, after the passing of time corresponding to $` \text{u} `$ slots (called the **transaction confirmation time**), all nodes, if queried and responding honestly, will report the transaction as stable.

### 1.1.2 Chains properties 

**Persistence** and **liveness** can be derived from basic **chain properties**, provided that the protocol structures the ledger as a **blockchain**—a sequential data structure. The following key chain properties ensure that the blockchain behaves securely and efficiently:

#### 1.1.2.1 **Common Prefix (CP)**: With parameter the **security parameter $`k \in \mathbb{N}`$**. 

Consider 2 chains $`C_1`$ and $`C_2`$ adopted by 2 honest parties at the onset of slots $`sl_1`$ and $`sl_2`$, respectively, where $`sl_1 \leq sl_2`$. The chains must satisfy the condition:

  ```math
  C_1^{\lceil k} \preceq C_2
  ```
  Where:
  - $`C_1^{\lceil k}`$ represents the chain obtained by removing the last $`k`$ blocks from $`C_1`$.
  - $`\preceq`$ denotes the **prefix relation**.

  This ensures that the shorter chain is a prefix of the longer one, ensuring consistency across honest parties.

#### 1.1.2.2 **Honest-Bounded Chain Growth (HCG)**: With parameters $`\tau \in (0, 1]`$ (speed coefficient) and $`s \in \mathbb{N}`$ (Minimum Honest Block Inclusion Interval). 

The Honest-Bounded Chain Growth (HCG) property ensures that the blockchain grows steadily with honest participation. It uses two key parameters: $`\tau`$, the **speed coefficient**, which dictates how quickly blocks are produced relative to time, and $`s`$, the **Minimum Honest Block Inclusion Interval**, which defines the smallest span of consecutive slots in which at least one honest block must be produced.

Consider a chain $`C`$ adopted by an honest party. Let:
  - $`sl_2`$ be the slot associated with the **last block** of $`C`$,
  - $`sl_1`$ be a prior slot where $`C`$ has an honestly-generated block.

If $`sl_2 \geq sl_1 + s`$, this means that the honest chain has passed through an interval of $`s`$ slots. In such a case, the number of blocks produced in $`C`$ after $`sl_1`$ must be at least $`\tau s`$, where $`\tau`$ represents the fraction of slots that produce blocks.

- **$`s`$ (Minimum Honest Block Inclusion Interval)**: This parameter guarantees that within any sequence of $`s`$ consecutive slots, at least one block must be generated by an honest party. The choice of $`s`$ ensures that the chain grows consistently and prevents adversaries from delaying block production for too long. A higher value of $`s`$ allows for longer intervals between honest block production, while a smaller $`s`$ increases the frequency at which blocks must be produced, tightening the chain growth requirements.

- **$`\tau`$ (Speed Coefficient)**: The speed coefficient $`\tau`$ defines the proportion of slots in the interval $`s`$ that are expected to produce blocks. For instance, if $`\tau = 0.5`$ and $`s = 10`$, then at least $`\tau s = 5`$ blocks must be produced by honest participants within those 10 slots. 

Together, $`\tau`$ and $`s`$ ensure that the chain grows at a steady pace, with a balance between how frequently blocks must be produced and how much time is allowed between honest block generations. The $`s`$ parameter plays a critical role in ensuring that honest contributions remain frequent enough to maintain chain security, while $`\tau`$ governs the actual rate of block production within those intervals.

#### 1.1.2.3 **Existential Chain Quality (∃CQ)**: With parameter $`s \in \mathbb{N}`$ (Minimum Honest Block Inclusion Interval). 

Consider a chain $`C`$ adopted by an honest party at the onset of a slot. For any portion of $`C`$ spanning $`s`$ prior slots, there must be at least one honestly-generated block within this portion. This ensures that the chain includes contributions from honest participants. In practical terms, $`s`$ defines the length of a "safety window" where honest contributions are guaranteed.

#### 1.1.2.4 **Chain Growth (CG)**: With parameters $`\tau \in (0, 1]`$ (speed coefficient) and $`s \in \mathbb{N}`$ (Minimum Honest Block Inclusion Interval).

The Chain Growth (CG) property is a more general concept that combines both the **speed of block production** and the **frequency of honest contributions**. It uses two parameters: $`\tau`$, the **speed coefficient**, which governs how fast the chain grows, and $`s`$, the **Minimum Honest Block Inclusion Interval**, which ensures that honest blocks are consistently produced within a given interval of slots.

Consider a chain $`C`$ held by an honest party at the onset of a slot. For any portion of $`C`$ spanning $`s`$ contiguous prior slots . The number of blocks in this portion must be at least $`\tau s`$. 
The parameter $`\tau`$ determines the fraction of slots in which blocks are produced. For example, if $`\tau = 0.5`$ and $`s = 10`$, there should be at least 5 blocks produced within that span.
  
For example, if $`\tau = 0.5`$ and $`s = 10`$, then at least $`\tau s = 0.5 \times 10 = 5`$ blocks must be produced over the span of those 10 slots. 

**Note**: **∃CQ** and **HCG** are combined to provide this more general notion of chain growth (CG) 


## 1.2 Leader Election in Praos

### 1.2.1 Oblivious Leader Selection

As Explained into [KRD017 - Ouroboros- A provably secure proof-of-stake blockchain protocol](https://eprint.iacr.org/2016/889.pdf), Praos protocol possesses the following basic characteristics : 
- **Privacy**: Only the selected leader knows they have been chosen until they reveal themselves, often by publishing a proof. This minimizes the risk of targeted attacks against the leader since other network participants are unaware of the leader's identity during the selection process.

- **Verifiable Randomness**: The selection process uses verifiable randomness functions (VRFs) to ensure that the leader is chosen fairly and unpredictably. The VRF output acts as a cryptographic proof that the selection was both random and valid, meaning others can verify it without needing to know the leader in advance.

- **Low Communication Overhead**: Since the identity of the leader is hidden until the proof is revealed, Oblivious Leader Selection can reduce communication overhead and minimize the chance of network-level attacks, such as Distributed Denial of Service (DDoS) attacks, aimed at preventing the leader from performing their role.

Based on her local view, a party is capable of deciding, in a publicly verifiable way, whether she is permitted to produce the next block, she is called a **slot leader**. Assuming the block is valid, other parties update their local views by adopting the block, and proceed in this way continuously. At any moment, the probability of being permitted to issue a block is proportional to the relative stake a player has in the system, as reported by the blockchain itself :
1. potentially, multiple slot leaders may be elected for a particular slot (forming a slot leader set); 
2. frequently, slots will have no leaders assigned to them; This defined by the **Active Slot Coefficient f**
3. a priori, only a slot leader is aware that it is indeed a leader for a given slot; this assignment is unknown to all the other stakeholders—including other slot leaders of the same slot—until the other stakeholders receive a valid block from this slot leader.


### 1.2.2 Application of Verifiable Random Function (VRF)

The VRF is used to introduce randomness into the protocol, making the leader election process unpredictable. It ensures that:
- A participant is privately and verifiably selected to create a block for a given slot.
- The VRF output is both secret (only known to the selected leader) and verifiable (publicly checkable).

If $`VRF^\text{Output}_\text{(participant,t)} `$ is less than her private $` \text{epoch}_e `$ threshold, the participant is eligible to produce a block, she becomes a Slot Leader for that particular $` \text{slot}_t `$. Her $` \text{SlotLeaderProof}_\text{t} `$ is added in the $` \text{BlockHeader}_\text{t} `$ and others participants have the ability to verify the proof.



| **Features** | **Mathematical Form** | **Description**  | 
|--------------|------------------|-----------------------|
| **Slot Leader Proof** | $`VRF^\text{Certification}_\text{(participant,t)} \equiv (VRF^\text{Proof}_\text{(participant,t)},VRF^\text{Output}_\text{(participant,t)}) \leftarrow VRF_\text{gen} \left( KeyVRF^\text{participant}_\text{private}, \text{slot}_t \, \|\| \, \eta_\text{e} \right) `$ | This function computes the leader eligibility proof using the VRF, based on the slot number and randomness nonce.       | 
| **Slot Leader Threshold** | $` \text{Threshold}^\text{participant}_\text{e} = (1 - ActiveSlotCoefficient )^\frac{\text{ActiveStake}^\text{e}_\text{participant}}{\text{ActiveStake}^\text{e}_\text{total}}  `$ | This function calculates the threshold for a participant's eligibility to be selected as a slot leader during $` \text{epoch}_e `$.   | 
| **Eligibility Check** | $` isEligible\left (t,participant ,\text{ActivesStake}^\text{e},\eta_\text{e}\right) = \frac{ toBoundedNatural  \circ  VRF^\text{Output}_\text{(participant,t)}}{\text{MaxBoundedNatural}} < \text{Threshold}^\text{participant}_\text{e} `$ |The leader proof is compared against a threshold to determine if the participant is eligible to create a block.         |
| **Verification**       | $` VRF_\text{verify} \left(  KeyVRF^\text{participant}_\text{public}, VRF^\text{Certification}_\text{(participant,t)}\right) = \text{slot}_t \, \|\| \, \eta_\text{e}  `$ | Other nodes verify the correctness of the leader proof by recomputing it using the public VRF key and slot-specific input.     | 
 
| Where | |
|-----------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| $` \text{slot}_t `$                   | The current slot number.                                                                                                          |
| $` \eta_\text{e} `$                   | Eta, The randomness nonce used in $` \text{epoch}_\text{e} `$, computed within the previous $` \text{epoch}_\text{e-1} `$.            |
| $` key_\text{private} `$              | The node's secret (private) key.                                                                                                  |
| $` key_\text{public} `$               | The node's public key.                                                                                                            |
| $` VRF_\text{gen} \left( key_\text{private}, \text{input} \right) \rightarrow (Proof,Output) `$ | Generate a Certification with input |
| $` VRF_\text{verify} \left( key_\text{private}, (Proof,Output) \right) \rightarrow Input  `$ | Verify a Certification with input |
| $` a \|\| b `$                        | The concatenation of $`a`$ and $`b`$.                                                 |
| $` \text{ActiveStake}^\text{e}_\text{participant} `$ | The stake owned by the participant used in $` \text{epoch}_\text{e} `$, computed within the previous $` \text{epoch}_\text{e-1} `$                                                                                              |
| $` \text{ActiveStake}^\text{e}_\text{total} `$       | The total stake in the system used in $` \text{epoch}_\text{e} `$, computed within the previous $` \text{epoch}_\text{e-1} `$                                                                                                  |
| $` ActiveSlotCoefficient`$                            | The active slot coefficient (referred as $`f`$), representing the fraction of slots that will have a leader and eventually a block produced.                                           |

<details>
  <summary>Consensus Layer Code Samples Relevant to This Section </summary>
  
```haskell
-- | Assert that a natural is bounded by a certain value. Throws an error when
-- this is not the case.
assertBoundedNatural ::
  -- | Maximum bound
  Natural ->
  -- | Value
  Natural ->
  BoundedNatural
assertBoundedNatural maxVal val =
  if val <= maxVal
    then UnsafeBoundedNatural maxVal val
    else error $ show val <> " is greater than max value " <> show maxVal

-- | The output bytes of the VRF interpreted as a big endian natural number.
--
-- The range of this number is determined by the size of the VRF output bytes.
-- It is thus in the range @0 ..  2 ^ (8 * sizeOutputVRF proxy) - 1@.
--
getOutputVRFNatural :: OutputVRF v -> Natural
getOutputVRFNatural = bytesToNatural . getOutputVRFBytes

-- | Check that the certified VRF output, when used as a natural, is valid for
-- being slot leader.
checkLeaderValue ::
  forall v.
  VRF.VRFAlgorithm v =>
  VRF.OutputVRF v ->
  Rational ->
  ActiveSlotCoeff ->
  Bool
checkLeaderValue certVRF σ f =
  checkLeaderNatValue (assertBoundedNatural certNatMax (VRF.getOutputVRFNatural certVRF)) σ f
  where
    certNatMax :: Natural
    certNatMax = (2 :: Natural) ^ (8 * VRF.sizeOutputVRF certVRF)

-- | Check that the certified input natural is valid for being slot leader. This
-- means we check that
--
-- p < 1 - (1 - f)^σ
--
-- where p = certNat / certNatMax.
--
-- The calculation is done using the following optimization:
--
-- let q = 1 - p and c = ln(1 - f)
--
-- then           p < 1 - (1 - f)^σ
-- <=>  1 / (1 - p) < exp(-σ * c)
-- <=>  1 / q       < exp(-σ * c)
--
-- This can be efficiently be computed by `taylorExpCmp` which returns `ABOVE`
-- in case the reference value `1 / (1 - p)` is above the exponential function
-- at `-σ * c`, `BELOW` if it is below or `MaxReached` if it couldn't
-- conclusively compute this within the given iteration bounds.
--
-- Note that  1       1               1                         certNatMax
--           --- =  ----- = ---------------------------- = ----------------------
--            q     1 - p    1 - (certNat / certNatMax)    (certNatMax - certNat)
checkLeaderNatValue ::
  -- | Certified nat value
  BoundedNatural ->
  -- | Stake proportion
  Rational ->
  ActiveSlotCoeff ->
  Bool
checkLeaderNatValue bn σ f =
  if activeSlotVal f == maxBound
    then -- If the active slot coefficient is equal to one,
    -- then nearly every stake pool can produce a block every slot.
    -- In this degenerate case, where ln (1-f) is not defined,
    -- we let the VRF leader check always succeed.
    -- This is a testing convenience, the active slot coefficient should not
    -- bet set above one half otherwise.
      True
    else case taylorExpCmp 3 recip_q x of
      ABOVE _ _ -> False
      BELOW _ _ -> True
      MaxReached _ -> False
  where
    c, recip_q, x :: FixedPoint
    c = activeSlotLog f
    recip_q = fromRational (toInteger certNatMax % toInteger (certNatMax - certNat))
    x = -fromRational σ * c
    certNatMax = bvMaxValue bn
    certNat = bvValue bn
  ```
</details>

### 1.2.3 Eligibility Check Input Variables   

For a participant to determine if they are eligible to be a slot leader, 2 key variable inputs are required:

```math 
isEligible\left (t,participant ,\text{ActivesStake}^\text{e},\eta_\text{e}\right) = \frac{ toBoundedNatural  \circ  VRF^\text{Output}_\text{(participant,t)}}{\text{MaxBoundedNatural}} < \text{Threshold}^\text{participant}_\text{e}
```

#### 1.2.3.1 Active Stake Distribution  
   - $`\text{ActivesStake}^\text{e}_\text{participant}`$: The participant's stake during epoch $`e`$.
   - $`\text{ActiveStake}^\text{e}_\text{total}`$: The total stake in the system during epoch $`e`$.

#### 1.2.3.2 The Randomness Nonces

The protocol operates with three distinct nonces, each serving a critical role in determining the eligibility of participants and maintaining security and randomness within the system:

1. **The Epoch eta nonce $`\eta_\text{e}`$**
   - This is the final nonce used to determine participant eligibility during epoch $`e`$.  
   - It originates from $`\eta_e^\text{candidate}`$ XOR with $`\eta_\text{evolving}`$ of the last block of the previous epoch , which becomes stabilized at the conclusion of $`\text{epoch}_{e-1}`$ and transitions into $`\text{epoch}_e`$  

```math
\eta_\text{e} = \eta^\text{candidate}_{e} \oplus \eta_\text{evolving}^{T_{\text{end}}^{\text{epoch}_{e-2}}} , \quad \text{when } t = T_{\text{start}}^{\text{epoch}_e} 
```
<details>
  <summary>**N.B** : divergence with academic papers </summary> 
  <p>it is :

```math
      \eta_\text{e} = \eta_\text{candidate}^{t^*} , \quad \text{when } t = T_{\text{start}}^{\text{epoch}_e} 
```
  Explain why : ...
  </p>
</details>

2. **The Epoch eta candidate nonce $`\eta_e^\text{candidate}`$**:  
   - It represents a candidate nonce for epoch $`e`$ and is the last derived  $`\eta_\text{evolving}^{t}`$ at the end of phase 2 $`\text{epoch}_{e-1}`$.   
   - The value of $`\eta_\text{e}`$ is derived from the $`\eta_e^\text{candidate}`$ contained within the fork that is ultimately selected as the **canonical chain** at the conclusion of $`\text{epoch}_{e-1}`$.  
   
```math
\eta_\text{e}^\text{candidate} = \eta_\text{evolving}^{t}, \quad \text{when } t = T_{\text{phase2}_\text{end}}^{\text{epoch}_{e-1}}  
```

3. **The Eta evolving nonce $`\eta_\text{evolving}`$**:  
   - This nonce evolves dynamically as the blockchain grows.  
   - For every block produced within the blockchain tree, a unique $`\eta_\text{evolving}`$ is computed :

```math
   \eta_{\text{evolving}}^{t+1} =
   \begin{cases}
   \text{ProtocolParameter}_\text{extraEntropy} & \text{if } t = 0, \\
   \eta_{\text{evolving}}^{t} \oplus VRF^\text{Output}_\text{t+1} & \text{else if BlockProduced}(t) \\
   \eta_{\text{evolving}}^{t}  & \text{otherwise.}
   \end{cases}
   
````
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

An epoch is structured to ensure that these two key input variables are distributively available to each participant, allowing for decentralized and secure leader election across the network.

### 1.2.4 Epoch Structure 

In Praos and Genesis, An epoch consists of 3 logical phases to compute these 2 key variables—**active stake distribution** and **randomness beacon**—by going through the following phases:

![Epoch Structure](epoch-structure-praos.png)

The sequential flow of these 3 phases is deliberately structured by designed : 

| Id | **Phase**                                                  | **Key Property**                 | **Description**| 
|----|-------------------------------|---------------------------|-------------------------------|
| **1.**| $`\text{ActiveStakeDistribution}_e `$ Stabilization | **Chain Growth (CG)**  | This phase must be long enough to satisfy the **Chain Growth (CG)** property, ensuring that each honest party's chain grows by at least $`k`$ blocks. This guarantees that all honest parties agree on the stake distribution from the previous epoch. | 
| **2.**| Honest Randomness in $`\eta_\text{e}`$     | **Existential Chain Quality (∃CQ)** | After the Active Stake Distribution being stabilized to prevent adversaries from adjusting their stake in their favor, this phase must be sufficiently long to satisfy the Existential Chain Quality (∃CQ) property, which is parameterized by $`s \in \mathbb{N}`$, ensuring that at least one honestly-generated block is included within any span of $s$ slots. The presence of such a block guarantees that honest contributions to the randomness used in the leader election process are incorporated. This phase directly improves the quality of the randomness $` \eta_\text{e} `$ by ensuring that adversarial attempts to manipulate the randomness beacon are mitigated. The honest block serves as a critical input, strengthening the unpredictability and fairness of the leader election mechanism.   | 
| **3.**| $`\eta_\text{e}`$ Stabilization   | **Chain Growth (CG)**          | This phase must again be long enough to satisfy the **Chain Growth (CG)** property, ensuring that each honest party's chain grows by at least $`k`$ blocks, allowing all honest parties to agree on the randomness contributions from the second phase. | 

### 1.2.5 Epoch & Phases Length 

While there is no theoretical upper bound on the epoch size—since it is defined by social and practical considerations (e.g., $`10 \, \text{k}/f`$ slots, approximately 5 days)—the epoch does have a defined lower bound. Phases 1 and 3 have fixed sizes of $`3 \, \text{k}/f`$ and $`4 \, \text{k}/f`$, respectively. The size of Phase 2, "Honest Randomness in $`\eta_\text{e}`$," is adjustable with a minimum size of $`1 \, \text{k}/f`$. 

The structure of an epoch is often described by the ratio `3:3:4`:
- **Phase 1** occupies **3** parts of the total epoch.
- **Phase 2** also occupies **3** parts of the epoch (adjusted slightly to ensure the total reaches **10** parts in total.). 
- **Phase 3** takes up the remaining **4** parts of the epoch.

## 1.3 Forks, Rollbacks, Finality and Settlement Times

With Ouroboros Praos, as with [Nakamoto consensus](https://coinmarketcap.com/academy/article/what-is-the-nakamoto-consensus) in general, transaction finality is probabilistic rather than immediate. This means a transaction isn't guaranteed being permanently stored in the ledger when it's first included in a block. Instead, each additional block added on top strengthens its permanence, gradually decreasing the likelihood of rollback. 

Ouroboros Praos guarantees that after 𝐾 blocks have been produced, the likelihood of a rollback diminishes to the point where those blocks can be regarded as secure and permanent within the Ledger. However, prior to the production of these 𝐾 blocks, multiple versions of the blockchain—commonly referred to as "forks"—may coexist across the network. Each fork represents a potential ledger state until finality is ultimately achieved.

The consensus layer operates with a structure that resembles a branching "tree" of blockchains before finality stabilizes :   
![alt text](high-level-ledger-structure.png)

Blockchain forks can occur for several reasons:

- Multiple slot leaders can be elected for a single slot, potentially resulting in the production of multiple blocks within that slot.
- Block propagation across the network takes time, causing nodes to have differing views of the current chain.
- Nodes can dynamically join or leave the network.
- An adversarial node is not obligated to agree with the most recent block (or series of blocks); it can instead choose to append its block to an earlier block in the chain.

Short forks, typically just a few blocks long, occur frequently and are usually non-problematic. The rolled-back blocks are often nearly identical, containing the same transactions, though they might be distributed differently among the blocks or have minor differences.

However, longer forks can have harmful consequences. 
For example, if an end-user (the recipient of funds) makes a decision—such as accepting payment and delivering goods to another user (the sender of the transaction)—based on a transaction that is later rolled back and does not reappear because it was invalid (e.g., due to double-spending a UTxO), 
it creates a risk of fraud.

# 2. Randomness Manipulation Objectives  

## 2.1 Exposure 

In its current version, Praos has a vulnerability where an adversary can exploit the nonce $`\eta_\text{e}`$, the random value used for selecting block producers. This allows the adversary to incrementally and iteratively undermine the uniform distribution of slot leaders, threatening the fairness and unpredictability of the leader selection process.

At the conclusion of phase 2, when the $\eta^\text{candidate}_{e}$ nonce is determined, the distribution of slot leaders for the next epoch becomes deterministic in a private manner. This means that, at this point, the adversary gains precise knowledge of the slots in which they will be elected but lacks detailed knowledge of the slot distribution for honest participants.

The window of opportunity opens just before the conclusion of phase 2. 

For example, if the adversary acts as the slot leader immediately before this phase transition, they can choose whether to produce a block or not. This decision grants them the ability to test and compare two different slot leader distributions for the upcoming epoch.  

This marks the beginning of a grinding attack, where the adversary's primary goal is to maximize the number of adversarial trailing blocks at this critical juncture. By doing so, they expand the range of potential slot leader distributions, significantly enhancing their influence over the protocol. In essence, the adversary gains access to $2^x$ possible combinations of slot leader distributions, where $x$ denotes the number of trailing leader slots at this particular stage of the protocol.

![alt text](grinding-opportunity-window.png)

We use the term "exposure" because the adversary cannot directly control their positioning for a grinding attack; it depends on the random distribution of leader slots. The likelihood of securing x trailing leader slots at the critical juncture decreases significantly as x increases. 

## 2.2 Slot Leader Distribution Selection

This is the pivotal moment where the adversary's prior efforts pay off. She is now in a position with *x* trailing blocks at the critical juncture. At this stage, the adversary can generate the `2^x` possible `η` nonces, compute the next epoch's slot leader distribution for each nonce, and strategically select the distribution that best aligns with her attack strategy. This positioning enables her to deploy the attack effectively in the subsequent epoch.

![alt text](image-1.png)

As the adversary accumulates trailing blocks, the limiting factor swiftly shifts to their computational power. This is why the attacker can be seen as gaining partial control over the protocol through their adversarial stake, but as an entry point that scales with their computational power, amplifying potential harm. To provide a sense of scale, let’s quantify the accumulated complexity of computing these slot leader distributions as the adversary accumulates trailing blocks : 

| **Exponent $`x`$**| **Number of possible distributions**        | **Complexity**                                            |
|-------------------|------------------------|-------------------------------------------------------|
| 0                 | $`2^0 = 1`$             | Single unit, no duplication.                         |
| 1                 | $`2^1 = 2`$             | Minimum branching or two choices.                    |
| 4                 | $`2^4 = 16`$            | Small-scale scenarios (e.g., a few trailing blocks).  |
| 8                 | $`2^8 = 256`$           | Moderate possibilities (e.g., basic attacks).        |
| 16                | $`2^{16} = 65,536`$     | Large-scale possibilities, extensive grinding.       |
| 20                | $`2^{20} = 1,048,576`$  | Highly intensive computational effort.               |
| 32                | $`2^{32} = 4,294,967,296`$ | Massive brute-force or exhaustive attack scale.      |
| 40                | $`2^{40} = 1,099,511,627,776`$ | Beyond realistic limits for most adversaries.        |

Accumulating x trailing leader positions is not just a statistical anomaly—it necessitates an underlying intent to exploit or destabilize the protocol. Achieving such a level of control requires significant coordination, making it highly unlikely to occur without deliberate adversarial motives. 

Once an attacker reaches this threshold, their objectives extend beyond a single exploit and diversify into various strategic threats. Below is a non-exhaustive list of potential attack vectors, ranging from minor disruptions in system throughput to severe breaches that compromise the protocol’s integrity and structure:

### Economic Exploitation
Manipulating slot leader distributions to prioritize transactions that benefit the adversary or to extract higher fees.

### Censorship Attacks
Selectively excluding transactions from specific stakeholders to suppress competition or dissent.

### Minority Stake Exploitation
Amplifying the influence of a small adversarial stake by targeting specific epoch transitions.

### Fork Manipulation
Creating and maintaining malicious forks to destabilize consensus or execute double-spend attacks.

### Settlement Delays
Strategically delaying block confirmation to undermine trust in the protocol's settlement guarantees.

### Double-Spend Attacks
Exploiting control over slot leader distributions to reverse confirmed transactions and execute double-spends.

### Chain-Freezing Attacks
Using nonce selection to stall block production entirely, halting the protocol and causing network paralysis.

# 3. Non-Exaustive Manipulation Stategy List

The Ethereum community recently published an insightful paper titled [*Forking the RANDAO: Manipulating Ethereum's Distributed Randomness Beacon*](https://eprint.iacr.org/2025/037). Since the system model used to analyze randomness manipulation in Ethereum is also applicable to Cardano, we will extensively reference their work to explore various manipulation strategies within the Cardano ecosystem. 


## 3.1 System Model

A block can exist in one of four states:  

- **H / Proposed** – The validator successfully proposes a valid block, which is accepted by the supermajority of validators and included in the canonical chain. Honest validators always follow this behavior in our analysis, while adversarial validators may consider alternative strategies, such as withholding blocks.  

- **R / Reorged** – The validator proposes a valid block, but it ends up on a non-canonical branch of the blockchain. This block is no longer considered part of the main chain by the supermajority of the stake.  

- **M / Missed** – The validator fails to publish a block during its designated slot. For an honest validator, this typically results from connectivity issues or other operational failures.  

- **P / Private** – The validator constructs a block but does not immediately publish it during its assigned slot. Instead, an adversarial validator selectively shares the block with validators within its staking pool. Later, the private block may be introduced into the canonical chain by **forking** the next block—a strategy known as an **ex-ante reorg attack**. Alternatively, depending on the evolving chain state, the attacker may decide to withhold the block entirely, a tactic we refer to as **regret**.  


![alt text](image-4.png)


Block statuses are denoted as $` H^e_i, R^e_i, M^e_i, P^e_i `$ indicating that the 
block in the $`i`$ th slot in epoch $`e`$ was proposed, reorged, missed, or built privately, respectively. Reorged and missed blocks do not contribute to the generation of $`\eta_e`$ since they are not part of the canonical chain. 

## 3.2 Self Mixing Strategy

The adversary can selectively propose or miss blocks to manipulate $`\eta_e`$. Assume that $`\mathcal{A}`$ is assigned with $`t`$ consecutive tail blocks, formally $`\mathcal{A}^{t}`$ of epoch $`e`$, then $`\mathcal{A}`$ can choose arbitrarily between $`2^t`$ $`\eta_e`$ by missing or proposing each tail block. Thus, it is trivial that $`\mathcal{A}^{t} \in AS_{\alpha}(m,n)`$ for $`0 \leq t \leq m`$, as $`\mathcal{A}`$ can compute $`\eta_e`$ corresponding to $`C^t`$.  

The manipulative power for $`t = 2`$ is the following decision tree 

![alt text](image-5.png)

e.g : The adversary chooses option $`\{H^e_{30}, M^e_{31}\}`$ if the calculated $`\eta_e`$ eventually leads to the highest number of blocks. In this case, sacrificing Slot 30 and 31 is worthwhile, as it results in a significantly higher number of blocks in epoch $`e + 2`$.  

## 3.3 Forking Strategies


To achieve the goal of maximizing $x$ trailing blocks at this critical juncture, the adversary leverages the forking nature of the consensus protocol by introducing a private chain. By strategically applying the Longest-Chain Rule to their advantage, the adversary ensures that the last honest trailing blocks are excluded at this pivotal moment. With this added dimension, gaining access to $2^x$ possible combinations of slot leader distributions becomes equivalent to $x = |A| - |H|$, where $|A|$ and $|H|$ represent the number of adversarial and honest blocks, respectively, within this specific interval of the protocol : 

<div align="center">
  <img src="grinding-opportunity.png" alt="Grinding Opportunity" width="600">
</div>

The adversary can choose between **two forking strategies** depending on when they act:

- **Preemptive Forking (Ex-Ante Reorging):** The adversary **forks before** the randomness update, ensuring that only adversarial VRF contributions are included while honest ones are discarded. This allows them to manipulate $`\eta_e`$ **before it is finalized**, biasing leader selection for the next epoch.

- **Reactive Forking (Ex-Post Reorging):** Instead of acting in advance, the adversary **waits until all honest VRF contributions are revealed** before deciding whether to fork. If the observed $`\eta_e`$ is unfavorable, they **publish an alternative chain**, replacing the honest blocks and modifying the randomness post-facto.

Both strategies undermine fairness in leader election, with **Preemptive Forking** favoring proactive randomness manipulation and **Reactive Forking** enabling selective, informed chain reorganizations.


## 4. The Quantification Challenge 

TODO

## Goals

<!-- A list of goals and non-goals a project is pursuing, ranked by importance. These goals should help understand the design space for the solution and what the underlying project is ultimately trying to achieve.

Goals may also contain requirements for the project. For example, they may include anything from a deadline to a budget (in terms of complexity or time) to security concerns.

Finally, goals may also serve as evaluation metrics to assess how good a proposed solution is. -->

    
The goal is to **mitigate or completely eliminate grinding attacks** on the protocol by introducing **targeted protocol enhancements** to address this issue. Two approaches are actively being explored to address the **Randomness Manipulation Problem**:  

- **Complete Elimination of Grinding Attacks** – Ongoing research aims to make the protocol fully resistant to such attacks. One notable example is *[Efficient Random Beacons with Adaptive Security for Ungrindable Blockchains](https://eprint.iacr.org/2021/1698.pdf).*  
- **Partial Mitigation by Increasing Attack Complexity** – While full protection may not yet be feasible, making such attacks **computationally and economically prohibitive** can significantly reduce their viability. This approach is the basis of the [**Phalanx CIP**](../CIP/Readme.md).   

However, while **fully protecting the protocol from Randomness Manipulation attacks** may not yet be feasible, it is crucial to advance in the following areas:  

- **Risk Quantification** : Assessing the **profitability and feasibility of attacks**, along with **refining risk assessment models**, will provide deeper insights into vulnerabilities and their potential impact on the protocol's security and stability.  

- **Transparency on Manipulations** : **Enhancing detection mechanisms**, such as **self-mixing analysis** and **forking manipulation detection**, can help identify potential exploits and assess ongoing threats in real time.  

- **Game Theory & Economic Disincentives** –   
  **Promoting stake operator diversity** and **strengthening decentralization incentives** will reduce the economic viability of manipulation, fostering a more **resilient and distributed** stake pool ecosystem.  

## Open Questions
<!-- A set of questions to which any proposed solution should find an answer. Questions should help guide solutions design by highlighting some foreseen vulnerabilities or design flaws. Solutions in the form of CIP should thereby include these questions as part of their 'Rationale' section and provide an argued answer to each. -->

<!-- OPTIONAL SECTIONS: see CIP-9999 > Specification > CPS > Structure table -->

- *How vulnerable is Cardano to Randomness Manipulation, and what are the potential consequences?* 
- *Is Cardano currently being manipulated?* 
- *Are we sufficiently discouraging randomness manipulation?* 
- How does handling the worst-case scenario of a grinding attack impact the security parameter $`K`$ in the Ouroboros Consensus Protocol


## References 
 
- [KRD017 - Ouroboros- A provably secure proof-of-stake blockchain protocol](https://eprint.iacr.org/2016/889.pdf)
- [DGKR18 -  Ouroboros Praos/ An adaptively-secure, semi-synchronous proof-of-stake blockchain](https://eprint.iacr.org/2017/573.pdf)
- [Practical Settlement Bounds For Longest Chain Consensus](https://eprint.iacr.org/2022/1571.pdf) 
- [The combinatorics of the longest-chain rule: Linear consistency for proof-of-stake blockchains](https://eprint.iacr.org/2017/241.pdf)
- [Efficient Random Beacons with Adaptive Securityfor Ungrindable Blockchains](https://eprint.iacr.org/2021/1698.pdf)
- [Forking the RANDAO: Manipulating Ethereum's Distributed Randomness Beacon](https://eprint.iacr.org/2025/037)



## Copyright
<!-- The CIP must be explicitly licensed under acceptable copyright terms.  Uncomment the one you wish to use (delete the other one) and ensure it matches the License field in the header: -->

This CIP is licensed under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/legalcode).
<!-- This CIP is licensed under [Apache-2.0](http://www.apache.org/licenses/LICENSE-2.0). -->
