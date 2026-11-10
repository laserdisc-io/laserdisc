# Subscribing

Pub/sub messages and `MONITOR` don't answer a request: the server keeps sending
them. A connection doing this can't be used for other commands, so
`RedisSubscriber` opens one just for that. You keep using `RedisClient` for
everything else, including `publish`.

```scala
import cats.effect.IO
import laserdisc.*
import laserdisc.auto.*
import laserdisc.fs2.*
import laserdisc.redis_8_10.*

val channels = OneOrMoreKeys.unsafeFrom(List(Key("orders")))

RedisSubscriber[IO].to("localhost", 6379, subscribe[String](channels)).use { messages =>
  messages.evalMap(message => IO.println(s"${message.channel}: ${message.value}")).compile.drain
}
```

Once the resource is open the server has confirmed the subscription, so anything
published from then on reaches you.

| Method | What you receive | Available on |
| --- | --- | --- |
| `subscribe[A](channels)` | `Message(channel, value)` | Every server |
| `psubscribe[A](patterns)` | `PatternMessage(pattern, channel, value)` | Every server |
| `ssubscribe[A](channels)` | `ShardMessage(channel, value)` | Redis and Valkey 7.0 and later, Dragonfly |
| `monitor` | `MonitorEvent(time, db, client, command)` | Every server |

`monitor` reports every command from every client. It slows the server down, so
use it for debugging.

## What to expect

- **The subscription is fixed.** You choose the channels when you open it, and
  closing it unsubscribes. To listen to more, open another and merge the streams.
- **Each subscription uses a connection.**
- **The stream fails if the connection drops.** Pub/sub doesn't keep messages, so
  anything published in the meantime is lost. Whether to open a new one is up to
  you.
- **The stream fails on a message it can't read** as the type you asked for. Ask
  for `Bulk` to get the raw bytes of any message.
- **In a cluster**, `ssubscribe` needs a connection to the node that serves the
  channel's slot.

If the server needs a password, pass it before the subscription:

```scala
RedisSubscriber[IO].to("localhost", 6379, Key("secret"), subscribe[String](channels))
```

## Knowing when keys change

Client tracking tells you when a key you've read changes, which is how you keep a
local cache of it. laserdisc reads protocol 2, so the notices go to a second
connection:

```scala
val c       = laserdisc.redis_8_10.client
val options = laserdisc.redis_8_10.servertypes.trackingOptions

RedisSubscriber[IO].identified("localhost", 6379, c.invalidations).use { case (id, notices) =>
  redisClient.send(c.tracking(options.redirect(id))) *>
    notices.evalMap(notice => IO.println(notice)).compile.drain
}
```

Each notice is `Invalidation.Keys(keys)`, or `Invalidation.Everything` after a
command like `FLUSHALL`.

- **Tracking doesn't survive a reconnect.** `RedisClient` reconnects on its own, and
  the new connection isn't tracking anything, without any notice saying so. Don't
  rely on a cache being fresh after a connection problem.
- **`caching` only applies to the very next command**, so send them together:
  `redisClient.send(c.caching(true), get[String]("key"))`. Otherwise another
  caller's command may come in between.
- **It's for the whole connection.** `RedisClient` shares one connection between all
  your calls, so what every caller reads gets tracked. Use a `RedisClient` of its own
  to track only some reads.
- **Dragonfly doesn't have it.** It only allows tracking with protocol 3.
