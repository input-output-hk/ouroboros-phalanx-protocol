# ouroboros-anti-grinding

Compiling chiavdf requires cmake, boost and GMP/MPIR. After compiling it, one can run the VDF and VDF accumulation benchmarks as follows

```bash
cd chiavdf/rust_bindings/
cargo build
cargo +nightly bench
```
