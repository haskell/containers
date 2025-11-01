#! /bin/bash

# convenience script to run all benchmarks for the master branch and for the
# starting branch, and compare the before and after in the output file
# bench-$CURR_BRANCH_NAME.out, in `benchmark_tmp/`

exitWith () {
  echo "$1"
  exit $(($2))
}

if [ -n "$(git status --porcelain)" ]; then
  echo "there are changes, exiting benchmark script";
  exit 1
fi

CURR=`git rev-parse --abbrev-ref HEAD`

if [ "$CURR" == "master" ]
then
  exitWith "current branch is master, ending benchmarking" -1
fi

BENCHMARKS=(
  intmap-benchmarks
  intset-benchmarks
  map-benchmarks
  tree-benchmarks
  sequence-benchmarks
  set-benchmarks
  graph-benchmarks
  set-operations-intmap
  set-operations-intset
  set-operations-map
  set-operations-set
  lookupge-intmap
  lookupge-map
)

BENCHMARK_TMP="benchmark_tmp"

mkdir -p $BENCHMARK_TMP

git checkout master

cabal build all || exitWith "master build errored" 2

MASTER_BENCH_LOG="$BENCHMARK_TMP/bench-master.log"
echo -n > $MASTER_BENCH_LOG

for BENCHMARK in "${BENCHMARKS[@]}"
do
  echo "running $BENCHMARK on master"
  (cabal bench $BENCHMARK --benchmark-options="--csv $BENCHMARK.csv" >> $MASTER_BENCH_LOG 2>&1) ||
    exitWith "benchmark $BENCHMARK failed to run on master, exiting" 3
done

git checkout $CURR

cabal build all || exitWith "$CURR build errored" 4

CURR_BENCH_LOG="$BENCHMARK_TMP/bench-$CURR.log"
echo -n > $CURR_BENCH_LOG

for BENCHMARK in "${BENCHMARKS[@]}"
do
  echo "running $BENCHMARK on $CURR"
  (cabal bench $BENCHMARK --benchmark-options="--csv $BENCHMARK-$CURR.csv --baseline $BENCHMARK.csv" >> $CURR_BENCH_LOG 2>&1) ||
    exitWith "benchmark $BENCHMARK failed to run on $CURR, exiting" 5
done

mv containers-tests/*.csv $BENCHMARK_TMP/
