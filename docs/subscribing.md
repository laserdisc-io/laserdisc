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
