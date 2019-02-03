# Scala collection benchmarks

This repo aims to provide a standardised JMH benchmark for comparing the performance
of Scala and Java collections. 


## Running

Make sure tests are all green, run:

    ./sbt test

To start benchmarking all modules:

    ./sbt benchmarkAll
    
The arguments to JMH is `-i 10 -wi 10 -f 1 -t 1 -rf json`, benchmarks will take ~1 hour per module.
The results will be written to the `docs` directory as JSON files with the `jmh_` prefix.

The `docs` directory has all the required files for the benchmark to be viewable in a browser. Start a web server in the `docs` directory and navigate to `report.html`

## Results

Latest JMH reports are [here](https://tom91136.github.io/scala-collection-benchmarks/report.html)


Benchmark machine:
```
TBA 
```
