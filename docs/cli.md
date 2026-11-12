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

If the server needs a password, add `-a` and the password after the port, and
`--user` and the user if there's one. To use a database other than 0, add `-n`
and its number:

```sh
sbt "cli/run localhost 6379 --user app -a secret -n 2"
```

Anyone on the same machine can see a password given this way in the list of
running programs.

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

Commands that block, such as `BLPOP`, wait on the server for as long as you
give them. With a wait of 0, they wait for as long as it takes:

```
localhost:6379> BLPOP jobs 0
```

To stop waiting, press Ctrl-C. This also leaves the prompt.

Each command gets one reply on the same connection, so the prompt refuses the
commands that would break that:

- `SUBSCRIBE`, `PSUBSCRIBE`, `SSUBSCRIBE` and `MONITOR`, as their messages keep
  coming after the reply. To receive them, see [Subscribing](subscribing.md).
- `UNSUBSCRIBE`, `PUNSUBSCRIBE` and `SUNSUBSCRIBE`, as they reply once for each
  channel.
- `SYNC`, `PSYNC` and `REPLCONF`, which are for replicas.
- `CLIENT REPLY OFF` and `CLIENT REPLY SKIP`, as the server would stop replying.
- `HELLO 3`, as the prompt reads RESP2 replies only.
- `QUIT`, as the server would close the connection.
- `AUTH` and `SELECT`, as the prompt opens a new connection if the server
  closes this one, and the new one wouldn't have them. Use `-a`, `--user` and
  `-n` instead.

To leave, press Ctrl-D.
