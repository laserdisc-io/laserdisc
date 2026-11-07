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

The client also reconnects when the server stops replying: once a reply is late
by more than its `send`'s timeout, the client drops the connection and opens a
new one.

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

This doesn't make it a Redis Cluster client: laserdisc doesn't send each key to
the node that holds it, or follow a cluster's `MOVED` and `ASK` redirects. On a
cluster, a command for a key held by another node fails with a `MOVED` error.

## Sending commands

`send` takes from 1 to 22 commands and sends them together. You get back one
result per command, in the same order:

```scala mdoc:compile-only
import cats.effect.IO

import laserdisc.*
import laserdisc.redis_8_10.*
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
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.fs2.*

def quickGet(client: RedisClient[IO]): IO[Maybe[Option[String]]] =
  client.send(get[String]("a"), 2.seconds)
```

When it times out, the whole `send` fails with `RequestTimedOut`, which names
the first command still without a reply. Commands that were still waiting to be
sent aren't sent at all.

## Loops

Some commands are meant to be sent again and again. SCAN replies with a cursor
to send back until the server is done, XRANGE reads a stream a page at a time,
and XREAD can follow a stream as entries are added. For these there are loops:
`scanAll`, `hscanAll`, `hscannovaluesAll`, `sscanAll`, `zscanAll`,
`zscannoscoresAll`, `xrangeAll` and `xrevrangeAll`, and, with the blocking
commands, `xreadFollow` and `xreadgroupFollow`. `stream` runs a loop and gives
you its values as a `Stream`:

```scala mdoc:compile-only
import cats.effect.IO

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.fs2.*

def userKeys(client: RedisClient[IO]): Stream[IO, Key] =
  client.stream(scanAll("user:*"))
```

Each command is sent once the reply to the one before it is in. A failed
command fails the stream. Each command gets the same timeout as a `send`, or the
one you pass after the loop, and for a blocking command its wait is added to it.

To make your own loop, give `Loop` where to start, the command to send from
there, and what to do with the reply: the values it gives, and where to go next,
or `None` to stop. This one reads a list 100 items at a time:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*

def wholeList(key: Key): Loop[String] =
  Loop(0L)(start => lrange[String](key, Index.unsafeFrom(start), Index.unsafeFrom(start + 99))) { (start, page) =>
    page -> (if (page.size < 100) None else Some(start + 100))
  }
```

For blocking commands, use `Loop.blocking` with how long each one waits.

## Blocking commands

Blocking commands, under `blocking`, wait on the server for something to
happen. While one waits, it holds the client's connection: commands sent after
it on the same client, from anywhere in your program, only get their replies
once it's done. Give blocking commands, and loops that follow streams, a client
of their own.

## Logging

The client logs through [log-effect](https://github.com/laserdisc-io/log-effect).
To see its logs, put a `LogWriter` in implicit scope, for example
`SyncLogWriter.consoleLog[IO]`. Without one, the client doesn't log.
