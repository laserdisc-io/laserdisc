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

A client has one connection, which every part of your program that uses the
client shares: commands from all of them go out on it in turn, and their replies
come back in the same order. Open it once and pass it around.

The host is a name or an address, IPv6 ones included, written without brackets,
such as `"::1"`.

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
when that command failed, so one failed command doesn't affect the others: see
[Errors](#errors).

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
sent aren't sent at all. A command that was sent may still have run on the
server: the client only stopped waiting for its reply.

## Errors

A command that fails gives a `Left` in its place in the results, holding one of
these:

| Error | What happened |
| --- | --- |
| `Err` | The server replied with an error, such as `WRONGTYPE` for a key of another type. Its `message` is the server's. |
| `RESPDecErr` | The reply couldn't be read as the type you asked for. Its `message` says why. |
| `ServerUnavailable` | The client couldn't reach the server: see [Reconnecting](#reconnecting). Its `cause` says why. |
| `ConnectionSetupRefused` | The server refused the password or the database: see [Logging in](#logging-in-and-choosing-a-database). |
| `ServerTerminatedConnection` | The connection closed, or the server reset it, before the reply came. Its `cause` is the socket's error, if there was one. |
| `ServerNotResponding` | The server stopped replying, so the client closed the connection. |
| `WatchLost` | The keys of a transaction were watched on a connection that's gone: see [Transactions](commands.md#transactions). |
| `WatchCleanupRefused` | As for `WatchLost`, and the server also refused to end the watch on the new connection, so the client closed that connection too. None of the transaction's commands ran. |

The `send` itself fails, instead of giving results, when it times out, with
`RequestTimedOut`, and when the client was already closed, with
`ClientTerminated`.

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
User names and passwords are `Key`s, LaserDisc's type for a string that isn't
empty. A password from your configuration is a `String`: turn it into a `Key`
with `Key.from`, which gives a `Left` if it's empty:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.fs2.*

def credentials(user: String, password: String): Either[String, Credentials] =
  for {
    name   <- Key.from(user)
    secret <- Key.from(password)
  } yield Credentials(name, secret)
```

Each connection logs in and selects the database before it sends anything else,
and that includes the connections the client opens when it reconnects. Don't
send `AUTH` or `SELECT` yourself: a new connection wouldn't have them.

If the server refuses the password or the database, the client stops: every
command fails with `ConnectionSetupRefused`, which says why. If it refuses only
for now, because it's loading its data, running a script or has too many
clients, the client connects again, as when it can't reach the server.

## Connecting with TLS

To encrypt the connection, pass `tls`. `Tls.system` trusts the certificates your
system trusts, as for a hosted Redis whose certificate a public authority
signed:

```scala mdoc:compile-only
import cats.effect.{IO, Resource}

import laserdisc.*
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
that trusts it. On the JVM, one comes from a Java key store holding the
authority's certificate:

```scala mdoc:compile-only
import cats.effect.{IO, Resource}
import fs2.io.file.Path
import fs2.io.net.Network

import laserdisc.*
import laserdisc.auto.*
import laserdisc.fs2.*

val client: Resource[IO, RedisClient[IO]] =
  Resource
    .eval(Network[IO].tlsContext.fromKeyStoreFile(Path("truststore.jks"), "changeit".toCharArray, "changeit".toCharArray))
    .flatMap(context => RedisClient[IO].to("redis.internal", 6380, tls = Some(Tls(context))))
```

If the server asks for a client certificate, put yours, with its key, in the
same key store. On Scala.js and Scala Native, fs2 builds a `TLSContext` from
that platform's own settings: see
[fs2's TLS documentation](https://fs2.io/#/io?id=tls).

If you have to connect with a name or an address that the certificate doesn't
have, pass `Tls(context, checkServerName = false)`. The certificate still has to
be signed by one you trust, but any server with such a certificate could then
pose as yours.

On Scala Native, TLS needs s2n-tls installed, and a certificate that fails a check
takes 10 to 30 seconds to fail: see [Platform notes](platform-notes.md).

## Several servers

`RedisClient[IO].toNodes` takes a non-empty set of addresses. The client uses
one server at a time, starting with the first. If it can't reach that server, or
the connection drops with commands waiting on it, it moves on to the next one. A
server closing a connection nothing was waiting on, as servers do with idle
clients, is connected to again:

```scala mdoc:compile-only
import cats.data.NonEmptySet
import cats.effect.{IO, Resource}

import laserdisc.*
import laserdisc.auto.*
import laserdisc.fs2.*

val client: Resource[IO, RedisClient[IO]] =
  RedisClient[IO].toNodes(NonEmptySet.of(RedisAddress("redis-1", 6379), RedisAddress("redis-2", 6379)))
```

The addresses are tried in the set's order: by host, then port, unless you
build the set with an `Order` of your own.

When none of the servers can be reached, it waits before trying them all again,
as described in [Reconnecting](#reconnecting).

`toNodes` takes `credentials`, `database`, `tls` and `reconnect` too, and uses
them for every server.

This doesn't make it a Redis Cluster client. A cluster spreads its keys across
its nodes, and a node answers a command for a key it doesn't hold with a
`MOVED` error naming the right node. LaserDisc doesn't send each key to the node
that holds it, or follow those redirects, so on a cluster such a command fails
with that error.


## Reconnecting

If the connection drops, commands still waiting for a reply get a `Left` right
away, with `ServerTerminatedConnection` when the server closed it, and the
client reconnects. Those commands may have run on the server: the connection
went before their replies came. Commands that hadn't been sent yet go out on
the new connection. While the client can't reach the server, commands fail at
once with `ServerUnavailable`. Its `cause` says why the last attempt failed,
such as the connection being refused or the TLS handshake failing.

A new connection is set up like the one before it. It logs in and selects the
database, then it's set again to what you last set on the connection with
`client.tracking`, `client.setname`, `connection.hello` with a name, or
`readonly`. If the server refuses one of these on the new connection, as with
tracking whose notices went to a connection that's gone, the client logs why
and carries on without it.

Between attempts the client waits 100 milliseconds at first, then twice as long
each time, up to 5 seconds. Once the server answers again, the next wait is
short again. Each wait is cut by up to half at random, so that many clients
don't all reconnect at the same moment. To wait differently, pass a
`Reconnect` with the first and the longest wait. The first has to be more than
0, and the longest at least the first, or `Reconnect` throws an
`IllegalArgumentException`:

```scala mdoc:compile-only
import cats.effect.{IO, Resource}
import scala.concurrent.duration.*

import laserdisc.*
import laserdisc.auto.*
import laserdisc.fs2.*

val patient: Resource[IO, RedisClient[IO]] =
  RedisClient[IO].to("localhost", 6379, reconnect = Reconnect(500.millis, 30.seconds))
```

The client also reconnects when the server stops replying: once a reply is late
by more than its `send`'s timeout, the client drops the connection and opens a
new one.

Connecting to a server gives up after 10 seconds, so a server that doesn't
answer doesn't hold the client up: commands then fail with `ServerUnavailable`,
whose `cause` is `ConnectTimedOut`. To choose another limit, pass
`connectTimeout`:

```scala mdoc:compile-only
import cats.effect.{IO, Resource}
import scala.concurrent.duration.*

import laserdisc.*
import laserdisc.auto.*
import laserdisc.fs2.*

val client: Resource[IO, RedisClient[IO]] = RedisClient[IO].to("localhost", 6379, connectTimeout = 2.seconds)
```

## Loops

Some commands are meant to be sent again and again. SCAN replies with a cursor
to send back until the server is done, XRANGE reads a stream a page at a time,
and XREAD can follow a stream as entries are added. For these there are loops:
`scanAll`, `hscanAll`, `hscannovaluesAll`, `sscanAll`, `zscanAll`,
`zscannoscoresAll`, `xrangeAll` and `xrevrangeAll`, and, among the blocking
commands, `blocking.xreadFollow` and `blocking.xreadgroupFollow`. `stream` runs
a loop and gives you its values as a `Stream`:

```scala mdoc:compile-only
import cats.effect.IO
import fs2.Stream

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
seconds and the default timeout can take up to 50 seconds. The wait can't be 0,
which would mean for as long as it takes.

While one waits, it holds the client's connection: commands sent after it on the
same client, from anywhere in your program, only get their replies once it's
done, and may time out first. Give blocking commands, and loops that follow
streams, a client of their own.

### Waiting for as long as it takes

To wait with no limit, use a dedicated client and the commands under
`blocking.forever`. A dedicated client sends one command at a time, each once
the one before it has returned, so it's meant for one caller. Besides those
commands, it sends any other:

```scala mdoc:compile-only
import cats.effect.IO

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.fs2.*

val jobs = OneOrMoreKeys.unsafeFrom(List(Key("jobs")))

RedisClient[IO].dedicated("localhost", 6379).use { client =>
  client.send(blocking.forever.blpop[String](jobs)).flatMap(job => IO.println(job))
}
```

A command that waits for as long as it takes has no timeout. Cancel it to stop
waiting: as the server is still waiting on it, the client closes the connection,
and the next command connects again. The shared client from `to` doesn't
compile with these commands.

If the server goes away without closing the connection, as when its machine
loses power, a command with no timeout only fails once the operating system
gives up on the connection. Its keep-alive checks take about two hours by
default on Linux and macOS.

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
