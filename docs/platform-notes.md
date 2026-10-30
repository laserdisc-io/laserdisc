# Platform notes

The API is the same on every Scala version and platform. These are the places
where they differ.

## Scala 2 and Scala 3

On Scala 2, the argument types such as `Key` and `PosInt` come from
[refined](https://github.com/fthomas/refined). On Scala 3, they come from
[Iron](https://github.com/Iltotore/iron). Code that uses them through
LaserDisc, with `from`, `unsafeFrom` and `.value`, works on both.

If you write your own refined types for use with LaserDisc, write them with
refined on Scala 2 and with Iron on Scala 3.

## Scala.js and Scala Native

`laserdisc-core` and `laserdisc-circe` work on the JVM, Scala.js and Scala
Native.

`laserdisc-fs2` works on the JVM and Scala.js. On Scala.js it runs on Node.js,
not in browsers, because it needs network sockets.

On Node.js, a burst of a few hundred commands sent at the same time can stall
the connection. Those commands time out, and the client then reconnects on its
own.

`laserdisc-fs2` isn't available on Scala Native yet, because one of its
dependencies isn't published for it.
