---
CPS: ??
Title: Ouroboros Randomness Generation Sub-Protocol - The coin-flipping Problem
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

⚠️ **IMPORTANT NOTICE** ⚠️  

🚨 **This repository is now frozen.** Further development, discussions, and updates are happening in the **official CIP repository**.  

🔗 **Follow the latest updates here:** [https://github.com/cardano-foundation/CIPs/pull/1009](https://github.com/cardano-foundation/CIPs/pull/1008)  

Please ensure all contributions, comments, and discussions take place in the official repository. 🚀  

----

## Abstract

A well-designed consensus protocol is inherently modular, consisting of multiple sub-protocols that collectively ensure security, efficiency, and decentralization. Among these, the Randomness Generation Sub-Protocol is crucial in addressing the Coin-Flipping Problem — the challenge of generating fair, unbiased, and unpredictable randomness in a distributed setting.

The objective of this CPS is to formally document the Coin-Flipping Problem and coordinate the development of CIPs aimed at mitigating and, if possible, fully resolving this challenge within the Ouroboros protocol.

This problem is particularly critical in **Ouroboros**, where randomness serves as a foundation for key sub-protocols such as **leader election**. Ensuring a **robust and tamper-resistant** randomness mechanism is essential to preserving the **security, fairness, and integrity** of the protocol.

To uphold Cardano’s **decentralized ethos**, the community must proactively mitigate these risks and **reduce the feasibility of biasing strategies**. Addressing this challenge requires answering key questions:

- **Is Cardano currently being manipulated?**  
  Strengthening **detection mechanisms**, such as **self-mixing analysis** and **forking manipulation detection**, can help **identify potential exploits** and assess ongoing threats.

- **Are we sufficiently disincentivizing randomness manipulation?**  
  Enhancing **stake operator diversity** and reinforcing incentives for **decentralization** will make manipulation **economically unviable**, fostering a **resilient and distributed** stake pool ecosystem.

- **How vulnerable is Cardano to these attacks, and what are the potential consequences?**  
  Improving **risk quantification** will provide deeper insight into **attack feasibility, vulnerabilities, and potential security gaps** within the protocol.

Beyond **detection, assessment, and quantification**, **protocol-level enhancements** must be explored to directly **reduce manipulation opportunities** and strengthen incentives for honest participation.

Finally, it is essential to recognize that **adversarial capabilities continually evolve**, making this an **ongoing challenge** that demands sustained **research, adaptation, and community-driven innovation**.

## Table of Contents

- [**Problem**](#problem)
- [**Goals**](#goals)
- [**Open Questions**](#open-questions)
- [**References**](#references)
- [**Copyright**](#copyright)

  
## Problem



## Goals

<!-- A list of goals and non-goals a project is pursuing, ranked by importance. These goals should help understand the design space for the solution and what the underlying project is ultimately trying to achieve.

Goals may also contain requirements for the project. For example, they may include anything from a deadline to a budget (in terms of complexity or time) to security concerns.

Finally, goals may also serve as evaluation metrics to assess how good a proposed solution is. -->

    
The goal is to **mitigate or completely eliminate grinding attacks** on the protocol by introducing **targeted protocol enhancements** to address this issue. Two approaches are actively being explored to address the **Randomness Manipulation Problem**:  

- **Complete Elimination of Grinding Attacks** – Ongoing research aims to make the protocol fully resistant to such attacks. One notable example is *[Efficient Random Beacons with Adaptive Security for Ungrindable Blockchains](https://eprint.iacr.org/2021/1698.pdf).*  
- **Partial Mitigation by Increasing Attack Complexity** – While full protection may not yet be feasible, making such attacks **computationally and economically prohibitive** can significantly reduce their viability. This approach is the basis of the **Phalanx CIP** (Coming soon)].   

However, while **fully protecting the protocol from Randomness Manipulation attacks** may not yet be feasible, it is crucial to advance in the following areas:  

- **Risk Quantification** : Assessing the **profitability and feasibility of attacks**, along with **refining risk assessment models**, will provide deeper insights into vulnerabilities and their potential impact on the protocol's security and stability.  

- **Transparency on Manipulations** : **Enhancing detection mechanisms**, such as **self-mixing analysis** and **forking manipulation detection**, can help identify potential exploits and assess ongoing threats in real time.  

- **Game Theory & Economic Disincentives** –   
  **Promoting stake operator diversity** and **strengthening decentralization incentives** will reduce the economic viability of manipulation, fostering a more **resilient and distributed** stake pool ecosystem.  

We strongly encourage the community to actively engage in addressing this challenge by contributing research, proposing solutions, and participating in discussions. Collaborative efforts will be crucial in refining detection mechanisms, strengthening protocol resilience, and ensuring the long-term security and fairness of Ouroboros.

## Open Questions
<!-- A set of questions to which any proposed solution should find an answer. Questions should help guide solutions design by highlighting some foreseen vulnerabilities or design flaws. Solutions in the form of CIP should thereby include these questions as part of their 'Rationale' section and provide an argued answer to each. -->

<!-- OPTIONAL SECTIONS: see CIP-9999 > Specification > CPS > Structure table -->

- *How vulnerable is Cardano to randomness manipulation, and what are the potential consequences?*  
- *Is Cardano currently being manipulated?*  
- *Are we effectively discouraging randomness manipulation?*  
- *How does handling the worst-case scenario of a grinding attack impact the security parameter $K$ in the Ouroboros consensus protocol?*  
- *Who stands to benefit from a grinding attack?*  
- *What are the practical limits of a grinding attack given the current computational power available on the market?*  
- *Are these randomness manipulation strategies economically viable?*  


## References 
 
- [KRD017 - Ouroboros- A provably secure proof-of-stake blockchain protocol](https://eprint.iacr.org/2016/889.pdf)
- [DGKR18 -  Ouroboros Praos/ An adaptively-secure, semi-synchronous proof-of-stake blockchain](https://eprint.iacr.org/2017/573.pdf)
- [Practical Settlement Bounds For Longest Chain Consensus](https://eprint.iacr.org/2022/1571.pdf) 
- [The combinatorics of the longest-chain rule: Linear consistency for proof-of-stake blockchains](https://eprint.iacr.org/2017/241.pdf)
- [Efficient Random Beacons with Adaptive Securityfor Ungrindable Blockchains](https://eprint.iacr.org/2021/1698.pdf)
- [Forking the RANDAO: Manipulating Ethereum's Distributed Randomness Beacon](https://eprint.iacr.org/2025/037)
- [Security of Proof-of-Stake Blockchains](https://search.worldcat.org/title/1336590866)


## Copyright
<!-- The CIP must be explicitly licensed under acceptable copyright terms.  Uncomment the one you wish to use (delete the other one) and ensure it matches the License field in the header: -->

This CIP is licensed under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/legalcode).
<!-- This CIP is licensed under [Apache-2.0](http://www.apache.org/licenses/LICENSE-2.0). -->
