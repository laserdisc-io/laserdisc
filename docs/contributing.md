# Contributing

## Running the tests

To run the tests for every Scala version and platform:

```sh
sbt +test
```

These don't need a server.

## Integration tests

The integration tests talk to real servers. Start Redis, KeyDB, Valkey and Dragonfly with Docker, at the versions CI uses:

```sh
docker run -p 6379:6379 -d redis:8.10.3-alpine
docker run -p 6380:6379 -d eqalpha/keydb@sha256:6537505c42355ca1f571276bddf83f5b750f760f07b2a185a676481791e388ac
docker run -p 6381:6379 -d valkey/valkey:9.1.3-alpine
docker run -p 6382:6379 --ulimit memlock=-1 -d docker.dragonflydb.io/dragonflydb/dragonfly:v2.0.2
docker run -p 6383:6379 --ulimit memlock=-1 -d docker.dragonflydb.io/dragonflydb/dragonfly:v2.0.2 --cluster_mode=emulated
```

The KeyDB is version 6.3.4. It's given by its digest because each of its version
tags covers only one processor type.

The TLS tests need a Redis that takes only TLS, on port 6390, with the test
certificates in `test-setup/tls`. From the repository's root:

```sh
docker run -p 6390:6379 -v "$PWD/test-setup/tls:/tls:ro" -d redis:8.10.3-alpine --port 0 --tls-port 6379 --tls-cert-file /tls/server.crt --tls-key-file /tls/server.key --tls-ca-cert-file /tls/ca.crt --tls-auth-clients no
```

They're skipped on Scala Native, which would need s2n-tls installed.

Some of them also need a cluster of each. The steps that start those are in
`.github/workflows/ci.yml`, named "Start up a Redis cluster" and so on: copy
their commands. The tests connect the nodes themselves.

Then run them for every Scala version and platform:

```sh
sbt +integration/test +integrationJS/test +integrationNative/test
```

### One server version

CI also tests each version's import against a server of that version. To do the
same for one version, give its import to `test-setup/matrix.sh`:

```sh
test-setup/matrix.sh redis_7_4
```

It starts the server and a cluster of it in containers, runs the tests for that
version, and removes the containers. To keep it clear of servers you already
run, move its ports with `LASERDISC_PORT_OFFSET`:

```sh
LASERDISC_PORT_OFFSET=40000 test-setup/matrix.sh valkey_9_0
```

## Coverage

Each published module has to reach 90% of statements and 80% of branches with
its own tests. Tests also
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

## Formatting and license headers

```sh
sbt scalafmtAll integration/scalafmtAll scalafmtSbt headerCreateAll
```

Every source file starts with the license header, which `headerCreateAll` adds.
To check formatting and headers without changing files, as CI does, use
`scalafmtCheckAll`, `integration/scalafmtCheckAll`, `scalafmtSbtCheck` and
`headerCheckAll`.

## The site

This site is built from the `docs` folder. To preview it locally:

```sh
sbt ++3.9.0 docs/tlSitePreview
```

Then open [http://localhost:4242](http://localhost:4242).
