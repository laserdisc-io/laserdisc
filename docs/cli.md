# Command line

`laserdisc-cli` is an interactive prompt for sending commands to a server, like
`redis-cli`.

From a checkout of this repository, start it with the server's host and port:

```sh
sbt "cli/run localhost 6379"
```

On Scala.js, build it first, then start it with Node.js:

```sh
sbt "++3.9.0" cliJS/fastLinkJS
node cli/.js/target/scala-3.9.0/laserdisc-cli-fastopt/main.js localhost 6379
```

Type a command and its arguments, separated by spaces, and press Enter:

```
localhost:6379> SET greeting "hello world"
<<< OK - [1.20ms]
localhost:6379> GET greeting
<<< "hello world" - [0.48ms]
```

Each reply is shown the way `redis-cli` shows it, followed by how long it took.

Arguments with spaces go in quotes. Double quotes understand escapes such as
`\n`, `\t` and `\x41`. Single quotes take everything literally, except `\'`.

To leave, press Ctrl-D.
