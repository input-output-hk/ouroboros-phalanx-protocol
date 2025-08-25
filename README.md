<h1 align="center">Ouroboros Phalanx</h1>
<h3 align="center">Breaking the Economics of Grinding Attacks</h3>


Ouroboros Phalanx is a protocol-level extension of **Ouroboros Praos** that tackles a subtle weaknesses in proof-of-stake blockchains: **grinding attacks**.
In Praos, adversaries can exploit their leadership slots to bias the randomness that drives future leader elections. While this does not make Praos insecure, it **inflates settlement times** because the system must be conservatively parameterized against such attacks.

Phalanx breaks this dynamic by making every grinding attempt **computationally expensive**. By embedding a verifiable delay function (VDF) into the randomness pipeline, Phalanx raises the cost of adversarial bias by orders of magnitude. This shifts the economics of attack: what was once a feasible strategy becomes prohibitively expensive, while honest participants experience negligible additional cost.

The result is a protocol that **preserves Ouroboros security guarantees** while also enabling **faster settlement times**, creating a more efficient and more robust foundation for Cardano.

This repository contains:  
- A local copy of [**CPS-0021: Ouroboros Randomness Manipulation**](/CPS/CPD/).  
- A local copy of [**CIP-0161: Ouroboros Phalanx – Breaking Grinding Incentives**](/CIP/).  
- A reference implementation of the **cryptographic primitive recommended** in these documents : It is a fork of [Chia's VDF](https://github.com/Chia-Network/chiavdf/tree/main) repository with minor modifications to support larger discriminant and adding accumulator functions and additional benchmarks.  
- Benchmarks and tests to evaluate this primitive, adapted from [Chia’s VDF](https://github.com/Chia-Network/chiavdf/tree/main).  

## Getting Started

You can read the local copies of the [CIP](./CIP/Readme.md) and [CPS](./CPS/Readme.md) included in this repository,  
or consult their official versions on the [Cardano Foundation CIPs repository](https://github.com/cardano-foundation/CIPs).  

This repository also provides an implementation of the **VDF-based cryptographic primitive** recommended in those documents.  
- If you are moving forward with implementing the Phalanx protocol, we **recommend using this implementation** as the reference.  
- Below you’ll find the instructions to build and run benchmarks on it.  

### Clone Repository
```bash
git clone https://github.com/input-output-hk/ouroboros-phalanx.git
cd ouroboros-phalanx
git submodule update --init --recursive
```

### Build Dependencies
Compiling `chiavdf` requires **cmake**, **boost**, and **GMP/MPIR**.  

**macOS** (via Homebrew):
```bash
brew install gmp boost cmake
```

**Debian/Ubuntu**:
```bash
sudo apt-get install libgmp-dev libboost-all-dev cmake
```

> You may need to update your `PATH`.

### Run Benchmarks
Move into the `chiavdf` directory and run the benchmark suite.  

- Default discriminant size: **4,096 bits**.  
- Class group elements (forms) are encoded in **388B**.  
- To use a different discriminant, update `BQFC_MAX_D_BITS` in `chiavdf/src/bqfc.h`.  
  - This automatically adjusts form size.  
  - Benchmarks and test files must be updated accordingly.  

#### Python benchmarks
We provide simple VDF benchmarks on different discriminant sizes in python. First, we need to create a virtual environment and generate a wheel.

```
python3 -m venv venv
source venv/bin/activate

pip install wheel setuptools_scm pybind11
pip wheel .
```

After installing the resulting wheel (`<>.whl` file) with pip and tabulate `pip install tabulate`, we can run the benchmarks as follows:
```
python3 python3 tests/benches.py
```

#### Rust benchmarks
We provide more comprehensive benchmarks, on both VDF functions as well as aggregation, with Rust code using Criterion. As `chiavdf` library is written in C++, we need the `nightly` flag to compile the bindings.

```bash
cd rust_bindings/
cargo build
cargo +nightly bench
```

Note that criterion saves the benchmarks output in `target/criterion` in the root directory and can be read with a HTML viewer, e.g. using firefox:
```
firefox target/criterion/report/index.html
```

## Who We Are
This project is developed by the **IOG Innovation Group**, with contributions from cryptographers, protocol engineers, and community reviewers.  

## Disclaimer
> **Important Disclaimer & Acceptance of Risk**  
> This repository contains **research code and specifications**. It has **not been security audited**.  
> It is provided *as is*, for research and educational purposes only.  
> **Do not use in production systems** without conducting your own thorough review.  
> By using this repository, you acknowledge and accept all associated risks.  

## License
This project is licensed under [Apache-2.0](LICENSE).  
