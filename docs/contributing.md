# Contributing

## Running the tests

To run the tests for every Scala version and platform:

```sh
sbt +test
```

These don't need a server.

## Integration tests

The integration tests talk to real servers. Start Redis, KeyDB, Valkey and Dragonfly with Docker:

```sh
docker run -p 6379:6379 -d redis:alpine
docker run -p 6380:6379 -d eqalpha/keydb
docker run -p 6381:6379 -d valkey/valkey:alpine
docker run -p 6382:6379 --ulimit memlock=-1 -d docker.dragonflydb.io/dragonflydb/dragonfly:latest
docker run -p 6383:6379 --ulimit memlock=-1 -d docker.dragonflydb.io/dragonflydb/dragonfly:latest --cluster_mode=emulated
```

Some of them also need a cluster of each. The steps that start those are in
`.github/workflows/ci.yml`, named "Start up a Redis cluster" and so on: copy
their commands. The tests connect the nodes themselves.

Then run them for every Scala version and platform:

```sh
sbt +integration/test +integrationJS/test +integrationNative/test
```

## Coverage

Each published module has to reach the minimums with its own tests. Tests also
count towards the modules they depend on, so run them in this order:

```sh
sbt clean coverage \
  core/test core/coverageReport \
  circe/test circe/coverageReport \
  fs2/test fs2/coverageReport \
  cli/test cli/coverageReport \
  laws/test integration/test laserdiscJVM/coverageAggregate
```

This includes the integration tests, so it needs the same servers. Each module's
report is written to its `target/scala-2.13/scoverage-report/index.html`, and the
report for all of them to `.jvm/target/scala-2.13/scoverage-report/index.html`.

## Formatting

```sh
sbt scalafmtAll integration/scalafmtAll scalafmtSbt
```

To check formatting without changing files, use `scalafmtCheckAll`,
`integration/scalafmtCheckAll` and `scalafmtSbtCheck`.

## The site

This site is built from the `docs` folder. To preview it locally:

```sh
sbt ++3.9.0 docs/tlSitePreview
```

Then open [http://localhost:4242](http://localhost:4242).
