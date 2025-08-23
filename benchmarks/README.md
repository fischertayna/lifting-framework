# Running Benchmark

## Native Environment

1. **Run the Criterion benchmark suite**

This step must be run **first**. It generates the raw measurements (`benchmark.json`), which are required for the following steps.

  ```bash
  cabal run benchmark-suite -- --json benchmark_output/benchmark.json
  ```

1. **Generate runtime metrics (CSV)**

This script converts the raw `benchmark.json` into `runtime_metrics.csv`.

```bash
python3 benchmarks/scripts/generateCSV.py
```

1. **Extract cache metrics**

Finally, extract memoization statistics into `cache_metrics.csv`.

```bash
cabal run cache-metrics-extractor
```

## Using the Pre-built Docker Image

If you don’t want to install Haskell locally, you can run the same steps in the container.

⚠️ Important: all commands must be run outside the container. The container will execute them inside its environment.

#### One-shot run (recommended)

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

#### Step-by-step run

If you prefer, you can also run the three commands individually (they must be run in this order):

```bash
# 1. Run benchmark suite
docker run -it -v "$(pwd)/benchmark_output:/lifting-framework/benchmark_output" \
  ghcr.io/fischertayna/lifting-framework:sblp25 \
  cabal run benchmark-suite -- --json /lifting-framework/benchmark_output/benchmark.json

# 2. Generate runtime metrics
docker run -it -v "$(pwd)/benchmark_output:/lifting-framework/benchmark_output" \
  ghcr.io/fischertayna/lifting-framework:sblp25 \
  python3 benchmarks/scripts/generateCSV.py

# 3. Extract cache metrics
docker run -it -v "$(pwd)/benchmark_output:/lifting-framework/benchmark_output" \
  ghcr.io/fischertayna/lifting-framework:sblp25 \
  cabal run cache-metrics-extractor
```

---

If you prefer an interactive workflow, you can start a container and modify files inside it:

```bash
docker run -it ghcr.io/fischertayna/lifting-framework:sblp25
```

Once inside the container, you may edit files and execute the same commands described in the Native Environment section.

#### Copying Results from an Existing Container

If you forgot to mount a volume, you can still retrieve the data:

```bash
# List containers and note the ID (or name)
docker ps -a

# Copy the folder to the current directory
docker cp <CONTAINER_ID>:/lifting-framework/benchmark_output ./benchmark_output
```

Now you have the benchmark_output directory locally, ready for further analysis.

---

# Results

#### Performance

Pre-calculated results can be found at runtime_metrics.csv inside the benchmark_output/runtime_performance folder.

#### Memoization

Pre-calculated results can be found at cache_metrics.csv inside the benchmark_output/memoization folder.

Graphics summarizing the finds can be found in both folders.
