# Running Benchmark

1. Execute benchmark using criterion

```cabal run benchmark-suite -- --json benchmark_output/benchmark.json```

2. generate runtime metrics

```python3 benchmarks/scripts/generateCSV.py```

3.   generate cache metrics

```cabal run cache-metrics-extractor```