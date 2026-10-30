# Client

`laserdisc-fs2` gives you `RedisClient`, which connects to a server and sends
commands.

## Connecting

`RedisClient[IO].to(host, port)` returns a `Resource`. The connection opens when
you use it and closes when you're done:

```scala mdoc:compile-only
import cats.effect.{IO, Resource}

import laserdisc.*
import laserdisc.auto.*
import laserdisc.fs2.*

val client: Resource[IO, RedisClient[IO]] = RedisClient[IO].to("localhost", 6379)
```

If the connection drops, commands still waiting for a reply get a `Left` right
away, and the client reconnects. While it can't reach the server, it tries again
every 3 seconds, and commands sent in the meantime fail with
`ServerUnavailable`.

## Several servers

`RedisClient[IO].toNodes` takes a set of addresses. The client uses one server
at a time. If it can't reach that server, or the connection drops, it moves on
to the next one:

```scala mdoc:compile-only
import cats.effect.{IO, Resource}

import laserdisc.*
import laserdisc.auto.*
import laserdisc.fs2.*

val client: Resource[IO, RedisClient[IO]] =
  RedisClient[IO].toNodes(Set(RedisAddress("redis-1", 6379), RedisAddress("redis-2", 6379)))
```

When none of the servers can be reached, it waits 3 seconds before trying them
all again.

## Sending commands

`send` takes from 1 to 22 commands and sends them together. You get back one
result per command, in the same order:

```scala mdoc:compile-only
import cats.effect.IO

import laserdisc.*
import laserdisc.all.*
import laserdisc.auto.*
import laserdisc.fs2.*

def setAndGet(client: RedisClient[IO]): IO[(Maybe[OK], Maybe[Option[PosInt]])] =
  client.send(set("a", 23), get[PosInt]("a"))
```

Each result is a `Maybe[A]`, which is an `Either[Throwable, A]`. It's a `Left`
when the server answers with an error or when the reply can't be read as the
type you asked for, so one failed command doesn't affect the others.

## Timeouts

A `send` times out if its replies haven't all arrived within 20 seconds. To
choose another timeout, pass it after the commands:

```scala mdoc:compile-only
import cats.effect.IO
import scala.concurrent.duration.*

import laserdisc.*
import laserdisc.all.*
import laserdisc.auto.*
import laserdisc.fs2.*

def quickGet(client: RedisClient[IO]): IO[Maybe[Option[String]]] =
  client.send(get[String]("a"), 2.seconds)
```

When it times out, the whole `send` fails with `RequestTimedOut`, which names
the first command still without a reply. Commands that were still waiting to be
sent aren't sent at all.

## Logging

The client logs through [log-effect](https://github.com/laserdisc-io/log-effect).
To see its logs, put a `LogWriter` in implicit scope, for example
`SyncLogWriter.consoleLog[IO]`. Without one, the client doesn't log.
