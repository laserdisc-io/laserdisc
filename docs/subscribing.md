# Subscribing

With pub/sub (publish/subscribe), clients send messages to named channels, and
every client subscribed to a channel receives them. The messages don't answer a
request: the server keeps sending them, as does `MONITOR`. A connection doing
this can't be used for other commands, so `RedisSubscriber` opens one just for
that. You keep using `RedisClient` for everything else, including `publish`.

```scala mdoc:compile-only
import cats.effect.IO

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.fs2.*

val channels = OneOrMoreKeys.unsafeFrom(List(Key("orders")))

val listening: IO[Unit] =
  RedisSubscriber[IO].to("localhost", 6379, subscribe[String](channels)).use { messages =>
    messages.evalMap {
      case Right(message)   => IO.println(s"${message.channel}: ${message.value}")
      // a message that couldn't be read as a String: why, and the message as it came
      case Left(unreadable) => IO.println(s"couldn't read ${unreadable.message}: ${unreadable.error.getMessage}")
    }.compile.drain
  }
```

Once the resource is open the server has confirmed the subscription, so anything
published from then on reaches you.

Each message comes as a `Right`. One that can't be read as the type you asked
for comes as a `Left` of `Unreadable`, with the reason and the message as it
arrived, and the stream goes on to the next one.

| Method | What you receive | Available on |
| --- | --- | --- |
| `subscribe[A](channels)` | `Message(channel, value)` | Every server |
| `psubscribe[A](patterns)` | `PatternMessage(pattern, channel, value)` | Every server |
| `ssubscribe[A](channels)` | `ShardMessage(channel, value)` | Redis 7.0 and later, every Valkey, Dragonfly |
| `monitor` | `MonitorEvent(time, db, client, command)` | Every server |

Shard channels, for `ssubscribe`, are channels that live on a single node of a
Redis Cluster, instead of on every node. `monitor` reports every command from
every client. It slows the server down, so use it for debugging.

## What to expect

- **The subscription is fixed.** You choose the channels when you open it, and
  closing it unsubscribes. To listen to more, open another and merge the streams.
- **Use the stream while the subscription is open.** Once it's closed, the
  stream fails with `ClientTerminated`, whether it was running or is run after.
- **Each subscription uses a connection.**
- **The stream fails if the connection drops.** Pub/sub doesn't keep messages, so
  anything published in the meantime is lost, and running the stream again fails
  the same way. Whether to open a new one is up to you.
- **The stream fails if the server stops answering**, even if the connection
  stays open, as when the network between you goes down. Every 30 seconds the
  server is sent a `PING`, and if it says nothing before the next one, the
  stream fails with `ServerNotResponding`. `monitor` isn't checked this way, as
  the server would report the `PING` as a command it received: a `monitor`
  stream only fails once the operating system gives up on the connection, which
  takes about two hours by default on Linux and macOS.
- **A message that can't be read doesn't stop the stream.** Anyone who can
  publish to your channels can send something that isn't the type you asked
  for. Ask for `Bulk` to get any message's value as raw bytes. A message on a
  channel whose name isn't a valid `Key`, like the empty one, comes as
  `Unreadable` whatever you ask for.
- **The stream fails if the server replies with an error.**
- **In a cluster**, `ssubscribe` needs a connection to the node that holds the
  shard channel.

If the server needs a password, pass it before the subscription, with the user
if there's one:

```scala mdoc:compile-only
import cats.effect.IO

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.fs2.*

val channels = OneOrMoreKeys.unsafeFrom(List(Key("orders")))

val subscription = RedisSubscriber[IO].to("localhost", 6379, Credentials("app", "secret"), subscribe[String](channels))
```

If the server refuses them, opening fails with `ConnectionSetupRefused`, which
says why, as with the client.

To connect with TLS, start from `withTls`, with a `Tls` as for the
[client](client.md):

```scala mdoc:compile-only
import cats.effect.IO

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.fs2.*

val channels = OneOrMoreKeys.unsafeFrom(List(Key("orders")))

val subscription =
  Tls.system[IO].flatMap(tls => RedisSubscriber[IO].withTls(tls).to("redis.example.com", 6380, subscribe[String](channels)))
```

## Knowing when keys change

Client tracking tells you when a key you've read changes, which is how you keep a
local cache of it. LaserDisc reads replies in RESP2, version 2 of the format
Redis replies in, where the notices can't share the connection, so they go to a
second one:

```scala mdoc:compile-only
import cats.effect.IO

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.fs2.*

def track(redisClient: RedisClient[IO]): IO[Unit] =
  // the subscriber's connection gives its id, which the client's tracking sends the notices to
  RedisSubscriber[IO].identified("localhost", 6379, client.invalidations).use { case (id, notices) =>
    redisClient.send(client.tracking(servertypes.trackingOptions.redirect(id))) *>
      notices.evalMap(notice => IO.println(notice)).compile.drain
  }
```

Each notice is `Right(Invalidation.Keys(keys))`, or `Right(Invalidation.Everything)`
after a command like `FLUSHALL`. A notice that can't be read comes as a `Left` of
`Unreadable`; to be safe, treat it as `Everything`.

- **Tracking doesn't survive a reconnect.** `RedisClient` reconnects on its own, and
  the new connection isn't tracking anything, without any notice saying so. Don't
  rely on a cache being fresh after a connection problem.
- **`caching` only applies to the very next command**, so send them together:
  `redisClient.send(client.caching(true), get[String]("key"))`. Otherwise another
  caller's command may come in between.
- **It's for the whole connection.** `RedisClient` shares one connection between all
  your calls, so what every caller reads gets tracked. Use a `RedisClient` of its own
  to track only some reads.
- **Dragonfly doesn't have it.** It only allows tracking with RESP3.
