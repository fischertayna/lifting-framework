# Running Benchmark

1. Create memoized_state.dat vmemoized_state.dat if not exists

```touch memoized_state.dat vmemoized_state.dat```

2. Execute benchmark enabling GC stats

```cabal bench --benchmark-options="+RTS -T -RTS"```
