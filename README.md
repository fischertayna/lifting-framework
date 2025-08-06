# Lifting-Framework 

<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXX) -->

An **interpreter-based framework** that integrates variational lifting and memoization to support efficient and reusable static analysis of evolving SPLs.
The code accompanies the paper:

> **“An Interpreter-based Framework for Static Analysis of Variability in Space and Time”**  
> *SBLP’25, September 22–26, 2025, Recife, PE Vieira, Alves and Teixeira* (to appear).

All artefact files are released under the terms of the [MIT licence](LICENSE).

---

## Quick start (one-liner)

```bash
docker run -it ghcr.io/fischertayna/lifting-framework:sblp25
```

The container image is pre-built from Dockerfile and contains everything—compiler toolchain, CUDD, generated parser, Cabal store—so reviewers can **reproduce all experiments without installing anything locally**.

---

## Manual Installation

If you prefer a local build (or wish to hack on the code), follow [INSTALL.md](INSTALL.md).

---

## Running the language tools

#### Generate the Frontend from the .cf Grammar definition


```bash
bnfc --haskell --name-space=Language.Frontend --output=src ./src/Language/Language.cf
```

#### Running an analysis

```bash
cabal run < src/Language/Analysis/Simple.lng
```

or

```bash
mkdir -p build
ghc -isrc src/Language/Interpreter/Driver.hs -o build/Driver
./build/Driver < src/Language/Analysis/Simple.lng
```

Interpreter entry points live in app/Main.hs; change the module imported
there (e.g. Driver.Typecheck, Driver.Base) to switch functionality.

#### Running Tests

```bash
cabal test --test-show-details=streaming
```

You can change what tests will be run in the file Spec.hs.

---

## Benchmarks & empirical evaluation

Full instructions are in [benchmarks/README.md](benchmarks/README.md).

The benchmark harness writes a cache_metrics.csv and runtime_metrics.csv containing raw numbers that feed
directly into the paper’s artefact evaluation package.

---

### Acknowledgements

- The project began as a proof-of-concept by Bruno Worm and has been further developed and enhanced under my maintenance.

- This code incorporates valuable input from Vander Alves, Leopoldo Teixeira, Rodrigo Bonifácio, and Paulo Borba.

- Portions of the code depend on CUDD 3.0.0 (by Fabio Somenzi) and BNFC 2.9.4.1.