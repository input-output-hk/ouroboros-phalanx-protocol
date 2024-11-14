---
CIP: ?
Title: Ouroboros Aegis - (CPS Faster Settlement)
Category: Consensus
Status: Proposed
Authors:
    - Nicolas Henin <nicolas.henin@iohk.io>
Implementors: []
Discussions:
    - https://github.com/cardano-foundation/CIPs/pull/?
Created: 2024-10-03
License: CC-BY-4.0
---



## Abstract

<!-- A short (\~200 word) description of the proposed solution and the technical issue being addressed. --> 

We propose the **Ourboros Aegis** Extension to the Ouroboros Praos Protocol as one of 3 approaches ([Peras](),[Aegis]() and [Atalanta]()) to improve its settlement times (see [Faster Settlement CPS](https://github.com/cardano-scaling/CIPs/blob/053a1c3a428d8f2270d6e961feeac5a44a3c8be3/CPS-0STL/README.md)): 

![alt text](program.png)

The theoretical estimates for Cardano mainchain settlement times are significantly influenced by the potential impact of a **"grinding attack."**. 
Particurlarly, The **security parameter k** has been substantially increased to secure the Praos Protocol against these attacks. 

To reduce this effect and achieve faster settlement times, this CIP proposes some modifications to the Praos Protocol that would make such attacks considerably more expensive for adversaries, with a negligible additional cost to honest participants. 

By increasing the cost of a grinding attack, the protocol limits adversaries, who possess a certain amount of computational power, forcing them to carry out a much weaker form of the attack.


## Motivation: why is this CIP necessary?

<!-- A clear explanation that introduces the reason for a proposal, its use cases and stakeholders. If the CIP changes an established design then it must outline design issues that motivate a rework. For complex proposals, authors must write a Cardano Problem Statement (CPS) as defined in CIP-9999 and link to it as the `Motivation`. -->

The Praos consensus protocol, a key component of the Ouroboros family, secures blockchain networks through verifiable randomness in slot leader elections. 

However, the protocol is vulnerable to a grinding attack, where an adversary attempts to manipulate or predict the randomness used in leader selection by systematically "grinding" through potential inputs. 

This increases the likelihood of an attacker becoming a slot leader, compromising the fairness and security of the protocol. This vulnerability not only affects the security of the network but also has a direct impact on settlement times. By influencing the leader selection process, attackers can delay or manipulate block production, leading overall settlement delays. 

The Ouroboros Praos protocol, in contrast to its predecessor Ouroboros Classic, uses a more lightweight procedure for generating randomness. Concretely, the randomness nonce for a particular epoch is obtained by including a VRF value in each block, and hashing a concatenation of these values from a particular part of the chain belonging to the previous epoch. 

The downside of this efficiency improvement in Ouroboros Praos is that it allows for a limited amount of adversarial behavior, called grinding, where the adversary attempts to evaluate multiple potential paths leading to different nonces, and aims to steer the protocol to arrive at a nonce that is most advantageous for him. In this context, we refer to the action of determining one such potential randomness nonce as a single grinding attempt.

In the security analysis of Ouroboros Praos, this threat is contained by estimating the computational costs of such a single grinding attempt, and, assuming an upper bound on the computational power of the adversary in the short window of time when these grinding attempts must be performed, upper-bounding the number of such attempts an adversary is capable of performing. The protocol is then parametrized (in particular, the settlement times are chosen) so that settlement is guaranteed even when taking this limited grinding into account. 

This CIP is proposing to make a single grinding attempt significantly more costly for the adversary, hence decreasing the number of grinding attempts that an adversary with the same resource budget is capable of. In addition to reinforcing the protocol against CPU resource-based attacks, this enhancement will also enable to eventually reduce the **security parameter k**. 

This CIP partially addresses the [Ouroboros Faster Settlement Problem Statemement](https://github.com/cardano-scaling/CIPs/blob/053a1c3a428d8f2270d6e961feeac5a44a3c8be3/CPS-0STL/README.md) and will benefit a wide range of use cases and stakeholders across the Cardano community, including:
- Partner chains
- Exchanges
- Bridges
- DApps
- High-value transactions


## Specification

### 1. Contextual Overview and Current Anti-Grinding Measures in Ouroboros Praos


To understand why anti-grinding mechanisms are necessary in Ouroboros Praos, it’s important to first explain how the Leader Election process works. In the Praos, the fairness and security of leader election are essential for maintaining the integrity of the blockchain. Grinding attacks target this very mechanism by trying to manipulate the randomness used in leader election, so the leader election process itself must be both fair and unpredictable. 

Let’s walk through how leader election works, followed by an explanation of the anti-grinding mechanism.

#### 1.1 Leader Election in Praos

##### 1.1.1 Key Notifiable Particularities of Praos

As Explained into [KRD017 - Ouroboros- A provably secure proof-of-stake blockchain protocol](https://eprint.iacr.org/2016/889.pdf), Praos protocol possesses the following basic characteristics : 

> Based on her local view, a party is capable of deciding, in a publicly verifiable way, whether she is permitted to produce the next block. Assuming the block is valid, other parties update their local views by adopting the block, and proceed in this way continuously. At any moment, the probability of being permitted to issue a block is proportional to the relative stake a player has in the system, as reported by the blockchain itself.

1. potentially, multiple slot leaders may be elected for a particular slot (forming a slot leader set); 
2. frequently, slots will have no leaders assigned to them; This defined by the **Active Slot Coefficient f**
3. a priori, only a slot leader is aware that it is indeed a leader for a given slot; this assignment is unknown to all the other stakeholders—including other slot leaders of the same slot—until the other stakeholders receive a valid block from this slot leader.

the **Verifiable Random Function (VRF)** plays a pivotal role in ensuring the security and fairness of the leader election process.

##### 1.1.2 Application of Verifiable Random Function (VRF)

The VRF is used to introduce randomness into the protocol, making the leader election process unpredictable. It ensures that:
- A participant is privately and verifiably selected to create a block for a given slot.
- The VRF output is both secret (only known to the selected leader) and verifiable (publicly checkable).

If the VRF output (Slot Leader Proof) is less than her private $ \text{epoch}_e $ threshold, the participant is eligible to produce a block, she becomes a Slot Leader for that particular $ \text{slot}_t $. Her $ \text{SlotLeaderProof}_\text{t} $ is added in the $ \text{BlockHeader}_\text{t}$ and others participants have the ability to verify the proof.

| **Features** | **Mathematical Form** | **Description**  | 
|--------------|------------------|-----------------------|
| **Slot Leader Proof** | $ \text{SlotLeaderProof}_\text{t} = VRF_\text{gen} \left( key_\text{private}, \text{slot}_t \, \|\| \, \eta_\text{e} \right) $ | This function computes the leader eligibility proof using the VRF, based on the slot number and randomness nonce.       | 
| **Slot Leader Threshold** | $ \text{Threshold}_\text{e} = \frac{\text{stake}^\text{e}_\text{participant}}{\text{stake}^\text{e}_\text{total}} \times f  $ | This function calculates the threshold for a participant's eligibility to be selected as a slot leader during $ \text{epoch}_e $.   | 
| **Eligibility Check** | $ \text{SlotLeaderProof}_\text{t} < \text{Threshold}_\text{e} $ |The leader proof is compared against a threshold to determine if the participant is eligible to create a block.         |
| **Verification**       | $ VRF_\text{verify} \left( key_\text{public}, \text{SlotLeaderProof}_\text{t}\right) = \text{slot}_t \, \|\| \, \eta_\text{e}   $ | Other nodes verify the correctness of the leader proof by recomputing it using the public VRF key and slot-specific input.     | 
 
| Where | |
|-----------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| $ \text{slot}_t $                   | The current slot number.                                                                                                          |
| $ \eta_\text{e} $                   | Eta, The randomness nonce used in $ \text{epoch}_\text{e} $, computed within the previous $ \text{epoch}_\text{e-1} $.            |
| $ key_\text{private} $              | The node's secret (private) key.                                                                                                  |
| $ key_\text{public} $               | The node's public key.                                                                                                            |
| $ VRF_\text{gen} \left( key_\text{private}, \text{input} \right) \rightarrow Proof $ | Generate a Proof with input |
| $ VRF_\text{verify} \left( key_\text{private}, proof \right) \rightarrow Input  $ | Generate a Proof with input |
| $ a \|\| b $                        | The concatenation of a and b.                                                 |
| $ \text{stake}^\text{e}_\text{participant} $ | The stake owned by the participant used in $ \text{epoch}_\text{e} $, computed within the previous $ \text{epoch}_\text{e-1} $                                                                                              |
| $ \text{stake}^\text{e}_\text{total} $       | The total stake in the system used in $ \text{epoch}_\text{e} $, computed within the previous $ \text{epoch}_\text{e-1} $                                                                                                  |
| $ f $                               | The active slot coefficient, representing the fraction of slots that will have a leader and eventually a block produced.                                           |

##### 1.1.3 VRF Inputs : Randomessness Beacon $ \eta_\text{e} $ and Stake Distribution details  

the **Randomness Beacon $ \eta_\text{e} $** is a vital part of the leader election mechanism, as it provides the randomness used to determine slot leaders for each epoch. The beacon needs to be unpredictable, decentralized, and resistant to manipulation, ensuring that the leader election process is fair and secure. $ \eta_\text{e} $ (for an $ \text{epoch}_e $) is computed using the $ \text{SlotLeaderProof}_\text{t} $ produced and added in the $ \text{BlockHeader}_\text{t}$ from the previous $ \text{epoch}_\text{e-1} $ :
    

##### Visual Summary


If the recomputed value matches the original VRF output, the block is considered valid and the leader is confirmed.


Deciding whether a participant is eligible to issue a block is done through a private test, executed locally using a **VRF**. This VRF must have strong security characteristics even in the setting of malicious key generation: specifically, if provided with an input that has high entropy, the output of the VRF is unpredictable even when an adversary has subverted the key generation procedure. We call such VRF functions “**VRF with unpredictability under malicious key generation**”

This function takes the current slot and a nonce, which is predetermined for an epoch :

```math
   \text{VRF}_\text{output} = \text{VRF}(sk_\text{VRF}, \text{slot}_t \, || \, \eta)
```

##### The Simulated Randomness Beacon : the nonce η (eta) for an epoch e

This hash function is applied to the concatenation of VRF values that are inserted into each block during the first 16k/f slots of an epoch that lasts 24k/f slots in entirety. (The “quiet” period of the final 8k/f slots in each epoch will ensure that the nonce is stable before the next epoch begins.) The verifiability of those values is a key property that we exploit in the proof.


```haskell
-- | Input to the verifiable random function. Consists of the hash of the slot
-- and the epoch nonce.
newtype InputVRF = InputVRF {unInputVRF :: Hash Blake2b_256 InputVRF}
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (NoThunks, ToCBOR)

instance SignableRepresentation InputVRF where
  getSignableRepresentation (InputVRF x) = hashToBytes x

-- | Construct a unified VRF value
mkInputVRF ::
  SlotNo ->
  -- | Epoch nonce (Eta)
  Nonce ->
  InputVRF
mkInputVRF (SlotNo slot) eNonce =
  InputVRF
    . Hash.castHash
    . Hash.hashWith id
    . runByteBuilder (8 + 32)
    $ BS.word64BE slot
      <> ( case eNonce of
             NeutralNonce -> mempty
             Nonce h      -> BS.byteStringCopy (Hash.hashToBytes h)
         )
```


- code places to look at 
 ```haskell
-- | Check whether this node meets the leader threshold to issue a block.
-- | pi =φf(αi)≜1−(1−f)^αi 
meetsLeaderThreshold ::
  forall c.
  PraosCrypto c =>
  ConsensusConfig (Praos c) ->
  LedgerView (Praos c) ->
  SL.KeyHash 'SL.StakePool c ->
  VRF.CertifiedVRF (VRF c) InputVRF ->
  Bool
meetsLeaderThreshold
  PraosConfig {praosParams}
  Views.LedgerView {Views.lvPoolDistr}
  keyHash
  rho =
    checkLeaderNatValue
      (vrfLeaderValue (Proxy @c) rho)
      getStakeProportion
      (praosLeaderF praosParams)
    where
      getStakeProportion =
        maybe 0 SL.individualPoolStake $
          Map.lookup keyHash poolDistr
      SL.PoolDistr poolDistr _totalActiveStake = lvPoolDistr


checkLeaderNatValue ::
  -- | Certified nat value
  BoundedNatural ->
  -- | Stake proportion
  Rational ->
  ActiveSlotCoeff ->
  Bool 

 -- | This module implements VRF range extension as described in
-- https://iohk.io/en/research/library/papers/on-uc-secure-range-extension-and-batch-verification-for-ecvrf/

checkLeaderNatValue
meetsLeaderThreshold
module Ouroboros.Consensus.Protocol.Praos.VRF (
    InputVRF
  , VRFUsage (..)
  , mkInputVRF
  , vrfLeaderValue
  , vrfNonceValue
  )
 ```
<!-- The technical specification should describe the proposed improvement in sufficient technical detail. In particular, it should provide enough information that an implementation can be performed solely on the basis of the design in the CIP. This is necessary to facilitate multiple, interoperable implementations. This must include how the CIP should be versioned, if not covered under an optional Versioning main heading. If a proposal defines structure of on-chain data it must include a CDDL schema in its specification.-->

### Agda Specification Changes

The changes to the Agda specification are found [here](https://github.com/IntersectMBO/ouroboros-consensus/tree/polina/anti-grinding/docs/agda-spec).
The main changes can be summarized as follows :

- `RandomnessStabilisationWindow` was renamed to `RandomnessStabilisationWindowPlusOne`, because this aligns better with the 
case splitting required for the changes. The implication is that `RandomnessStabilisationWindowPlusOne` is exactly 
one slot more than the `RandomnessStabilisationWindow`

- The following modules are parametrized by the function `grindingf : Nonce → Nonce` that gets applied repeatedly to 
the pre-nonce at the arrival of each block, called `f` in the Quick Wins document. This function is left abstract
in the specification. 
  - `Spec.ChainHead` , `Spec.ChainHead.Properties` , `Spec.Protocol` , `Spec.Protocol.Properties` , `Spec.UpdateNonce` , `Spec.UpdateNonce.Properties`

- The field `pre-η`, representing the current value of the pre-nonce, is added to the the following states :
  - `UpdateNonceState` , `PrtclState` , `ChainHeadState`

**`UPDN` Transition.** This transition is called by the `PTCL` transition every time a new block is being applied. 
It specifies 

- how the evolving nonce `ηv` is evolved by a VRF result from the incoming block header, i.e. in every 
case, it is combined  with the result of the VRF calculation `η` 
 
- how the candidate nonce `ηc` is updated : it is kept fixed whenever the current slot 
is less than `RandomnessStabilisationWindowPlusOne` slots away from the first slot of the next epoch, 
and is set equal to the evolving nonce otherwise

This transition is changed from having two rules, `Update-Both` and `Only-Evolve`, which 
correspond to whether `s + RandomnessStabilisationWindowPlusOne` is `<` or `≥` than `firstSlot (sucᵉ (epoch s))` (respectively) 
to having three separate cases. In all cases, the evolving nonce `ηv` is evolved by applying the VRF result `η`, denoted `ηv * η` :

1. `Update-All`, corresponding to `≡`
  - old pre-nonce `pre-η` is discarded, and the value in this field is set to a new pre-nonce, which is the same as 
  the updated evolving nonce, `ηv * η`,
  - the old candidate nonce `ηc` is discarded, and the value in this field is set to `grinding pre-η`. This is the 
  result of the application of the anti-grinding function `grindingf` 

2. `New-PreN`, corresponding to `<`
  - old pre-nonce `pre-η` is updated to the result of the application of the anti-grinding function `grindingf`, to 
  the existing pre-nonce to get `grindingf pre-η` 
  - the new candidate nonce `ηc` is discarded, and the field value is set to the same as the updated pre-nonce, i.e. `grindingf pre-η` 

3. `Keep-PreN`, corresponding to `>`
  - old pre-nonce `pre-η` is updated to the result of the application of the anti-grinding function `grindingf`, to 
  the existing pre-nonce to get `grindingf pre-η` 
  - the candidate nonce `ηc` is kept constant, because it needs to remain fixed for this last part of the epoch

These rule changes ensure that a fresh pre-nonce (which comes from the VRF-based evolving nonce) is set in the 
slot that is `RandomnessStabilisationWindowPlusOne`
before the first slot of the next epoch, updated by applying the anti-grinding function `grindingf` for each 
block in the span of the subsequent epoch. Once that is complete, the updated pre-nonce becomes the new 
candidate nonce.

**Changes to other Transitions.**
The rule that calls `UPDN`, which is `PRTL`, still does so by requiring the correct execution of `UPDN`.
This is achieved by including the the start state of  `UPDN` the pre-nonce `pre-η`, and in its end state, the updated 
`pre-η'`, which is the update computed by `UPDN`. The pre-nonce is also included in the state updated 
by `PRTL` itself. Changes to rule that calls `PRTL`, which is `CHAINHEAD`, follow the same pattern 
as the changes to `PRTL` : including the pre-nonce in its state, and updating it by calling the `PRTL` 
rule to compute the update.

**NOTE** The rule `TICKN` is used to swap out the old epoch nonce 
at the start of the new epoch. This rule does not swap the previous epoch's nonce for the candidate 
nonce directly, but instead, it combines the candidate nonce with the hash of previous 
epoch's last block header to obtain the new epoch nonce. This rule remains unchanged, however, 
this divergence from the original design impacts the analysis of the effectiveness of the 
new anti-grinding measures.


## Rationale: how does this CIP achieve its goals?

> [!NOTE] Quantifying : 
> 1. The increase of security
> 2. the decrease security parameter k

<!-- The rationale fleshes out the specification by describing what motivated the design and what led to particular design decisions. It should describe alternate designs considered and related work. The rationale should provide evidence of consensus within the community and discuss significant objections or concerns raised during the discussion.

It must also explain how the proposal affects the backward compatibility of existing solutions when applicable. If the proposal responds to a CPS, the 'Rationale' section should explain how it addresses the CPS, and answer any questions that the CPS poses for potential solutions.
-->

## Path to Active

### Acceptance Criteria
<!-- Describes what are the acceptance criteria whereby a proposal becomes 'Active' -->

### Implementation Plan
<!-- A plan to meet those criteria or `N/A` if an implementation plan is not applicable. -->

<!-- OPTIONAL SECTIONS: see CIP-0001 > Document > Structure table -->
## References

- [KRD017 - Ouroboros- A provably secure proof-of-stake blockchain protocol](https://eprint.iacr.org/2016/889.pdf)
- [DGKR18 - Ouroboros Praos/ An adaptively-secure, semi-synchronous proof-of-stake blockchain](https://eprint.iacr.org/2017/573.pdf)
- [Practical Settlement Bounds For Longest Chain Consensus](https://eprint.iacr.org/2022/1571.pdf) 
- [On UC-Secure Range Extension and Batch Verification for ECVRF](https://eprint.iacr.org/2022/1045.pdf)
## Definitions

### Security Parameter k

Persistence with parameter k ∈ N. Once a node of the system proclaims a certain transaction
tx in the stable part of its ledger, the remaining nodes, if queried, will either report tx in the
same position of that ledger or report a stable ledger which is a prefix of that ledger. Here the
notion of stability is a predicate that is parameterized by a security parameter k; specifically, a
transaction is declared stable if and only if it is in a block that is more than k blocks deep in
the ledger.

## Copyright
<!-- The CIP must be explicitly licensed under acceptable copyright terms.  Uncomment the one you wish to use (delete the other one) and ensure it matches the License field in the header: -->

<!-- This CIP is licensed under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/legalcode). -->
<!-- This CIP is licensed under [Apache-2.0](http://www.apache.org/licenses/LICENSE-2.0). -->