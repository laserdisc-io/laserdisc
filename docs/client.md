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

Connecting to a server gives up after 10 seconds, so a server that doesn't
answer doesn't hold the client up. To choose another limit, pass
`connectTimeout`:

```scala mdoc:compile-only
import cats.effect.{IO, Resource}
import scala.concurrent.duration.*

import laserdisc.auto.*
import laserdisc.fs2.*

val client: Resource[IO, RedisClient[IO]] = RedisClient[IO].to("localhost", 6379, connectTimeout = 2.seconds)
```

## Logging in and choosing a database

If the server needs a password, pass `credentials`. To use a database other than
0, pass `database`:

```scala mdoc:compile-only
import cats.effect.{IO, Resource}

import laserdisc.*
import laserdisc.auto.*
import laserdisc.fs2.*

val client: Resource[IO, RedisClient[IO]] =
  RedisClient[IO].to("localhost", 6379, credentials = Some(Credentials("app", "secret")), database = Some(DbIndex(2)))
```

`Credentials("secret")`, with only a password, logs in as the default user.

Each connection logs in and selects the database before it sends anything else,
and that includes the connections the client opens when it reconnects. Don't
send `AUTH` or `SELECT` yourself: a new connection wouldn't have them.

If the server refuses the password or the database, the client stops: every
command fails with `ConnectionSetupRefused`, which says why.

## Connecting with TLS

To encrypt the connection, pass `tls`. `Tls.system` trusts the certificates your
system trusts, as for a hosted Redis whose certificate a public authority
signed:

```scala mdoc:compile-only
import cats.effect.{IO, Resource}

import laserdisc.auto.*
import laserdisc.fs2.*

val client: Resource[IO, RedisClient[IO]] =
  Tls.system[IO].flatMap(tls => RedisClient[IO].to("redis.example.com", 6380, tls = Some(tls)))
```

The client checks that a certificate it trusts signed the server's, and that the
server's certificate is for the host you connect to. So connect with the name on
the certificate. If the server's certificate fails either check, the client
sends nothing: your commands fail, and the client logs why and tries again.

To trust a certificate authority of your own, give `Tls` an fs2 `TLSContext`
that trusts it, such as one from `Network[IO].tlsContext.fromKeyStoreFile` on
the JVM.

If you have to connect with a name or an address that the certificate doesn't
have, pass `Tls(context, checkServerName = false)`. The certificate still has to
be signed by one you trust, but any server with such a certificate could then
pose as yours.

On Scala Native, TLS needs s2n-tls installed: see [Platform notes](platform-notes.md).

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

`toNodes` takes `credentials`, `database` and `tls` too, and uses them for
every server.

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
one you pass after the loop.

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

## Blocking commands

Blocking commands, under `blocking`, wait on the server for something to
happen. The client adds that wait to the `send`'s timeout, so `blpop` with 30
seconds and the default timeout can take up to 50 seconds. A wait of 0 means for
as long as it takes on the server, but the `send` still times out after its own
timeout.

While one waits, it holds the client's connection: commands sent after it on the
same client, from anywhere in your program, only get their replies once it's
done, and may time out first. Give blocking commands, and loops that follow
streams, a client of their own.

## Logging

The client logs through [log-effect](https://github.com/laserdisc-io/log-effect).
To see its logs, put a `LogWriter` in implicit scope, for example
`SyncLogWriter.consoleLogUpToLevel(LogLevels.Info)`. Without one, the client
doesn't log.

At info and above, the client logs its connections and their failures. At
trace, it also logs each command it sends and each reply it gets, but only the
command's name and how many arguments it has, and the kind and size of the
reply, or an error's message: never their values, which can be passwords or
your data.
