# Running Benchmark

## Native Environment

1. **Run the Criterion benchmark suite**

  ```bash
  cabal run benchmark-suite -- --json benchmark_output/benchmark.json
  ```

2. **Generate runtime metrics (CSV)**

```bash
python3 benchmarks/scripts/generateCSV.py
```

3. **Extract cache metrics**

```bash
cabal run cache-metrics-extractor
```

## Using the Pre-built Docker Image

Skip the local Haskell toolchain and run everything from the container:

```bash
docker run -it ghcr.io/fischertayna/lifting-framework:sblp25
```

You can, then, execute the same commands as the above Native Environment section

### Persisting Results to the Host

All benchmark artefacts are written to /app/benchmark_output inside the container.
Mount a host directory to keep them after the container exits:

```bash
docker run -it \
  -v "$(pwd)/benchmark_output:/app/benchmark_output" \
  ghcr.io/fischertayna/lifting-framework:sblp25 \
  cabal run benchmark-suite -- --json /app/benchmark_output/benchmark.json
```

You can also generate directly all the metrics:

```bash
docker run -it \
  -v "$(pwd)/benchmark_output:/app/benchmark_output" \
  ghcr.io/fischertayna/lifting-framework:sblp25 \
  python3 benchmarks/scripts/generateCSV.py
```

and

```bash
docker run -it \
  -v "$(pwd)/benchmark_output:/app/benchmark_output" \
  ghcr.io/fischertayna/lifting-framework:sblp25 \
  cabal run cache-metrics-extractor
```

After the run you will find benchmark_output/benchmark.json (and the CSV created by `generateCSV.py`) in the directory you mounted on your machine.

### Copying Results from an Existing Container

If you forgot to mount a volume, you can still retrieve the data:

```bash
# List containers and note the ID (or name)
docker ps -a

# Copy the folder to the current directory
docker cp <CONTAINER_ID>:/app/benchmark_output ./benchmark_output
```

Now you have the benchmark_output directory locally, ready for further analysis.

---

# Results

#### Performance

The results can be found at runtime_metrics.csv inside the benchmark_output/runtime_performance folder

#### Memoization

The results can be found at cache_metrics.csv inside the benchmark_output/memoization folder

Graphics summarizing the finds can be found in both folders
