# Lifting-Framework &nbsp;•&nbsp; Artifact for SBLP ’25

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16763257.svg)](https://doi.org/10.5281/zenodo.16763257)

An **interpreter-based framework** that combines **variational lifting** and **memoization** to enable scalable, reusable static analyses of evolving software-product lines (SPLs).

This repository contains **all files, code, data, and documentation required to build, test, and reproduce the results** reported in the paper:

> **“An Interpreter-based Framework for Static Analysis of Variability in Space and Time”**  
> *Tayná Vieira, Vander Alves & Leopoldo Teixeira* — *29ᵗʰ Brazilian Symposium on Programming Languages (SBLP’25), 22–26 Sep 2025, Recife, Brazil*.  
> [PDF available on GitHub](https://github.com/fischertayna/lifting-framework/blob/main/An_Interpreter-based_Framework_for_Static_Analysis_of_Variability_in_Space_and_Time_SBLP25.pdf)

---

## License and Distribution Rights

All **source code** is released under [MIT licence](LICENSE).  
The **paper PDF** and any artwork are released under [CC-BY 4.0](LICENSE-PAPER).  

---

## Repository Layout

```text
.
├── LICENSE # MIT license (code)
├── LICENSE-PAPER # CC-BY 4.0 license (Paper PDF and artwork)
├── [README.md](README.md) # You are here
├── [INSTALL.md](INSTALL.md) # Instructions for installing all requirements
├── Dockerfile # Reproducible container build
├── app/ # Main entry points (Driver, CLI tools)
├── src/ # Library + interpreters
│ ├── Base/ # Base common code
│ ├── Language/ # Interpreters code
│ │  ├── Analysis # Supported analysis
│ │  │  └── DFA # Analises described on paper
│ │  ├── Frontend # bnfc generated files
│ │  ├── Interpreter # Base Interpreter
│ │  ├── MInterpreter # Memoized Interpreter
│ │  ├── Typechecker # Base Interpreter
│ │  ├── VInterpreter # Variability-aware Interpreter
│ │  └── VMemoInterpreter # Variability-aware and Memoized Interpreter
│ ├── Memoization/ # Memoization common code
│ ├── Variability/ # Variability-aware common code
│ └── WhileLang # Necessary files to encode While Language
├── benchmarks/ # Evaluation programs and scripts
│ ├── scripts/ # script to generate the csv
│ └── [README.md](benchmarks/README.md) # instruction to how execute the benchmark
├── benchmark_output/ # Results of evaluation of the benchmark
│ ├── memoization/ # Metrics and graphics of cache
│ ├── raw_outputs/ # Raw resuls of benchmark evaluation using criterion
│ └── memoization/ # Metrics and graphics of runtime performance
└── test/ # HSpec unit tests
```

---

## Requirements

| Environment | Mandatory | Recommended minimum | Notes |
|-------------|-----------|---------------------|-------|
| **Option A – Docker** | Docker ≥ 20.10 | 2 vCPU / 4 GB RAM / 5 GB disk | Fastest path for reviewers – **no local toolchain needed**. |
| **Option B – Native build** | • GHC 9.2.x<br>• Cabal 3.10<br>• BNFC 2.9.4.1<br>• CUDD 3.0.0 headers & libs | Same as above + GNU Make | Verified on Ubuntu 22.04 LTS and macOS 14. |

> **Storage footprint**: the fresh checkout is ≈ 55 MB; a full native build (including Cabal store, generated code and benchmarks) needs ≈ 1.4 GB.

No external data subject to privacy, ethics, or legal constraints are included.

---

## Installation

### 1. Quick start (one-liner)

```bash
docker run -it ghcr.io/fischertayna/lifting-framework:sblp25
```

The container image is pre-built from Dockerfile and contains everything—compiler toolchain, CUDD, generated parser, Cabal store—so reviewers can **reproduce all experiments without installing anything locally**.

### 2. Manual Installation

If you prefer a local build (or wish to hack on the code), follow [INSTALL.md](INSTALL.md).

---

## Basic Usage

Analyses are implemented in src/Language/Analysis, with Data Flow Analyses located in src/Language/Analysis/DFA. All analyses must be written in PCF+, though you are free to define additional analyses in this language.

Programs to be analyzed should be expressed in the While Language, as described in Principles of Program Analysis. This language has been extended to support variability. For further details, see the paper “An Interpreter-based Framework for Static Analysis of Variability in Space and Time”.

Examples of While programs can be found in:

- src/WhileLang/WhileDFAExamples.hs

- benchmarks/ComplexExamples.hs

#### Interpreters

The framework provides four execution modes, each with its own driver module:

- Driver.Base – baseline interpreter

- Driver.Memo – interpreter with memoization

- Driver.Var – variability-aware interpreter

- Driver.VMemo – variability + memoization (default)

The active interpreter is selected in app/Main.hs by changing the imported driver. Each driver defines its own main function. To run a different program, adjust the input directly in the chosen driver module.

#### Smoke test

As a quick test, you can run the Fibonacci example (src/Language/Analysis/Fibonacci.lng):

```bash
cabal run lifting-framework < src/Language/Analysis/Fibonacci.lng
```

This program defines a variational input: the variable evaluates to 2 under presence condition A and to 3 under ¬A, represented as:

```
VarInteger (Var [(2, pcA), (3, notBDD pcA)])
```

Expected output:

```haskell
(VarInteger {int = [(2,Prop "~A"),(1,Prop "A")]},[(FuncKey {funcName = "fib", funcArgsHash = -4093578937204566083},(VarInteger {int = [(2,Prop "~A"),(1,Prop "A")]},0))])
```

The tuple contains:

1. Result – the variational integer produced by the analysis.
2. Final cache – key/value pairs stored by the interpreter’s memoization layer.

#### Run the unit test suite

```bash
cabal test --test-show-details=streaming
```

You can change what tests will be run in the file Spec.hs.

#### Compile the front-end from the grammar

```bash
bnfc --haskell --name-space=Language.Frontend --output=src ./src/Language/Language.cf
```

---

## Benchmarks & empirical evaluation

#### Quick run

To reproduce the benchmarks and persist results to your host machine, run:

```bash
docker run -it \
  -v "$(pwd)/benchmark_output:/lifting-framework/benchmark_output" \
  ghcr.io/fischertayna/lifting-framework:sblp25 \
  bash -c "
    cabal run benchmark-suite -- --json /lifting-framework/benchmark_output/benchmark.json &&
    python3 benchmarks/scripts/generateCSV.py &&
    cabal run cache-metrics-extractor
  "
```

This single command will:

1. **Run the Criterion benchmark suite** – produces benchmark.json.

2. **Generate runtime metrics (CSV)** – produces runtime_metrics.csv.

3. **Extract cache metrics** – produces cache_metrics.csv.

All results will appear in ./benchmark_output on your machine.

⏱️ Note: Running the full benchmark takes about **25 minutes** on a standard laptop.

#### Resulting files

| File                          | Description                        |
| ----------------------------- | ---------------------------------- |
| `benchmark_output/runtime_metrics.csv` | Wall-clock runtime per interpreter |
| `benchmark_output/cache_metrics.csv`   | Cache hits, misses, and reuse      |

Further details are in [benchmarks/README.md](benchmarks/README.md).

---

### Acknowledgements

- The project began as a proof-of-concept by Bruno Worm and has been further developed and enhanced under my maintenance.

- This code incorporates valuable input from Vander Alves, Leopoldo Teixeira, Rodrigo Bonifácio, and Paulo Borba.

- Portions of the code depend on CUDD 3.0.0 (by Fabio Somenzi) and BNFC 2.9.4.1.