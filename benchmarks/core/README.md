# Core benchmarks

[JMH](https://github.com/openjdk/jmh) benchmarks of `laserdisc-core`: writing
commands, and reading replies from bytes.

| Benchmark | What it measures |
| --- | --- |
| `ProtocolBenchStr` | Encoding a command with a few arguments, and decoding its simple string reply |
| `ProtocolBenchArr` | Decoding array replies, with and without nils, nested, and as pairs |
| `RESPBench` | Encoding and decoding each kind of reply |
| `RESPFrameBench` | Finding where each reply ends in the bytes that arrive, including large ones |
| `RESPParamWriteBench` | Turning a command's arguments into strings |
| `UTF8EncodingBench` | Encoding and decoding UTF-8 text, compared with the JDK's own |

To run one, from the repository's root:

```sh
sbt "core-bench/Jmh/run -i 20 -wi 10 -f 2 -t 1 .*RESPBench.*"
```

`-i` and `-wi` are the measured and the warm-up iterations, `-f` the number of
forks and `-t` the number of threads. The last argument picks the benchmarks by
name: leave it out to run them all.

The client's benchmarks, under `benchmarks/fs2`, run the same way with
`fs2-bench/Jmh/run`, and need Redis on `localhost:6379`.
