# Command line

`laserdisc-cli` is an interactive prompt for sending commands to a server, like
`redis-cli`.

Start it with the server's host and port. With
[coursier](https://get-coursier.io/), it runs straight from its published
artifact:

```sh
cs launch io.laserdisc:laserdisc-cli_3:@VERSION@ -M laserdisc.cli.CLI -- localhost 6379
```

From a checkout of this repository, run it with sbt:

```sh
sbt "cli/run localhost 6379"
```

On Scala Native, run `cliNative/run` the same way. It needs
[s2n-tls](https://github.com/aws/s2n-tls) installed, as with `brew install s2n`,
even without `--tls`: see
[Scala.js and Scala Native](platform-notes.md#scalajs-and-scala-native). On Scala.js, build it first,
then start it with Node.js:

```sh
sbt "++3.9.0" cliJS/fastLinkJS
node cli/js/target/scala-3.9.0/laserdisc-cli-fastopt/main.js localhost 6379
```

If the server needs a password, add `-a` (or `--password`) and the password
after the port, and `--user` and the user if there's one: `--user` needs a
password too. To use a database
other than 0, add `-n` (or `--database`) and its number:

```sh
sbt "cli/run localhost 6379 --user app -a secret -n 2"
```

Anyone on the same machine can see a password given this way in the list of
running programs. To keep it out of that list, set it in the `REDISCLI_AUTH`
environment variable instead, as with `redis-cli`:

```sh
export REDISCLI_AUTH=secret
sbt "cli/run localhost 6379 --user app -n 2"
```

A password given with `-a` is used instead of `REDISCLI_AUTH`. A password or a
user can't start with `-` after `-a` or `--user`, so that a forgotten value
isn't taken from the next option; use `REDISCLI_AUTH` for a password that does.

If the server refuses the password or the database, the prompt shows why and
stops.

To connect with TLS, add `--tls`. The server's certificate then has to be
signed by an authority your system trusts, and be for the host you typed. If
your server's certificate comes from an authority of your own, add `--cacert`
and the file holding that authority's certificate, in PEM:

```sh
sbt "cli/run redis.internal 6380 --tls --cacert ca.pem"
```

`--insecure` connects without checking the server's certificate at all. Use it
only to try things out: anyone between you and the server could pose as it.

`--help` lists the options. A mistake in them shows what's wrong, followed by
the same list.

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

Each command waits for its reply for as long as it takes, as with `redis-cli`.
Commands that block, such as `BLPOP`, wait on the server for as long as you
give them, and with a wait of 0, for as long as it takes:

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
- `HELLO 3`, as the prompt reads replies in [RESP2](commands.md#replies), and
  `HELLO 3` would switch to RESP3.
- `RESET`, as it would log out and go back to database 0.
- `QUIT`, as the server would close the connection.
- `AUTH`, `HELLO` with `AUTH`, and `SELECT`, as the prompt opens a new
  connection if the server closes this one, and the new one wouldn't have them.
  Use `-a`, `--user` and `-n` instead.

`MULTI`, `EXEC` and `WATCH` work as with `redis-cli`. If the connection is
replaced while a transaction or a watch is open, as when the server closes idle
clients, the server has dropped them. The commands you type next then fail
with `TransactionLost`, or `MULTI` with `WatchLost`, without being sent, until
`EXEC` or `DISCARD` ends the transaction, so that nothing runs outside it.

To leave, press Ctrl-D.
