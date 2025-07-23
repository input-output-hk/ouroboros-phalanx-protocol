# Ouroboros Phalanx
This repository include the anti-grinding project's Cardano Project Statement (CPS) and Cardano Improvement Proposal (CIP) as well as source code for evaluating and benchmarking VDF solutions (which is a fork of [Chia's VDF](https://github.com/Chia-Network/chiavdf/tree/main) repository with minor modifications to support larger discriminant and adding accumulator functions and additional benchmarks).

## VDF

## Installing Chia's VDF
First, we need to retrieve the code from the submodule, using the following command,
```
git submodule update --init --recursive
```

Compiling chiavdf requires cmake, boost and GMP/MPIR.

On MAC, we can use `homebrew` to directly install them:
```
brew install gmp
brew install boost
brew install cmake
```

On Debian systems,
```
sudo apt-get install libgmp-dev
sudo apt-get install libboost-all-dev
sudo apt-get install cmake
```

You may need to update the PATH.

### Running Benchmarks

Now that the required libraries are installed, we can move to `chiavdf` directory and run benchmarks.

We changed the maximum discriminant size to 4,096 bits. As such, all class group elements, or _forms_, -regardless of the actual discriminant- are encoded in 388B. If you wish to use, or benchmark, different discriminants, you may change the `BQFC_MAX_D_BITS` constant in `chiavdf/src/bqfc.h` file which will automatically update the form size. This size will need to be updated in the benchmarks and tests files accordingly.

#### Python benchmarks
We provide simple VDF benchmarks on different discriminant sizes in python. First, we need to create a virtual environment and generate a wheel.

```
python3 -m venv venv
source venv/bin/activate

pip install wheel setuptools_scm pybind11
pip wheel .
```

After installing the resulting wheel (`<>.whl` file) with pip, we can run the benchmarks as follows:
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
