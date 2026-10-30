# Contributing

## Running the tests

Some tests talk to real servers. Start Redis, KeyDB and Valkey with Docker:

```sh
docker run -p 6379:6379 -d redis:alpine
docker run -p 6380:6379 -d eqalpha/keydb
docker run -p 6381:6379 -d valkey/valkey:alpine
```

Then run the tests for every Scala version and platform:

```sh
sbt +test
```

## Coverage

```sh
sbt coverage core/test laws/test fs2/test circe/test cli/test laserdiscJVM/coverageAggregate
```

The report is written to `.jvm/target/scala-2.13/scoverage-report/index.html`.

## Formatting

```sh
sbt scalafmtAll scalafmtSbt
```

To check formatting without changing files, use `scalafmtCheckAll` and
`scalafmtSbtCheck`.

## The site

This site is built from the `docs` folder. To preview it locally:

```sh
sbt ++3.9.0 docs/tlSitePreview
```

Then open [http://localhost:4242](http://localhost:4242).
