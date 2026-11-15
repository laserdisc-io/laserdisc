# Platform notes

The API is the same on every Scala version and platform. These are the places
where they differ.

## Scala 2 and Scala 3

On Scala 2, the argument types such as `Key` and `PosInt` come from
[refined](https://github.com/fthomas/refined). On Scala 3, they come from
[Iron](https://github.com/Iltotore/iron). Code that uses them through
LaserDisc, with `from`, `unsafeFrom` and `.value`, works on both.

If you write your own types with constraints, like `Key`, for use with
LaserDisc, write them with refined on Scala 2 and with Iron on Scala 3.

The examples in these pages use `*` imports, such as `import laserdisc.*`. On
Scala 2 they need `scalacOptions += "-Xsource:3"`, or `_` in place of `*`. On
Scala 2.12, they also need `"-Ypartial-unification"`, as cats does there, for
`stream` to know the kind of stream to give.

## Scala.js and Scala Native

`laserdisc-core`, `laserdisc-circe`, `laserdisc-fs2` and `laserdisc-cli` work on
the JVM, Scala.js and Scala Native.

On Scala.js, `laserdisc-fs2` and `laserdisc-cli` run on Node.js, not in browsers,
because they need network sockets.

On Scala Native, connecting with TLS needs [s2n-tls](https://github.com/aws/s2n-tls)
installed where your program is built and where it runs, as fs2 uses it for
TLS. Without TLS, you don't need it, except for `laserdisc-cli`, which needs it
on Scala Native whether you use `--tls` or not.

On Scala Native, a connection whose server certificate fails a check takes 10
to 30 seconds to fail, as s2n-tls waits that long on purpose before saying so.
Commands sent meanwhile wait, and may time out, and closing the client waits
for that time to pass.

On Node.js 26, the connection can stop receiving replies when several arrive
at once. The commands waiting for those replies time out, and the client then
reconnects on its own. A subscription's stream stops getting messages the same
way, and fails with `ServerNotResponding` once its ping goes unanswered. Node.js 25 and earlier aren't affected.
