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

<!-- The technical specification should describe the proposed improvement in sufficient technical detail. In particular, it should provide enough information that an implementation can be performed solely on the basis of the design in the CIP. This is necessary to facilitate multiple, interoperable implementations. This must include how the CIP should be versioned, if not covered under an optional Versioning main heading. If a proposal defines structure of on-chain data it must include a CDDL schema in its specification.-->



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