# JSON with circe

`laserdisc-circe` stores values as JSON and reads them back, using
[circe](https://circe.github.io/circe/).

```scala
libraryDependencies += "io.laserdisc" %%% "laserdisc-circe" % "@VERSION@"
```

Import `laserdisc.interop.circe.*`. To store a value as JSON, turn it into a
circe `Json` with `asJson`, and read it back as `Json`:

```scala mdoc:compile-only
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.interop.circe.*

final case class User(name: String, age: Int)

implicit val userEncoder: Encoder[User] = Encoder.forProduct2("name", "age")(u => (u.name, u.age))
implicit val userDecoder: Decoder[User] = Decoder.forProduct2("name", "age")(User.apply)

val save = set("user", User("Ada", 36).asJson)
val load = get[Json]("user")
```

To write and read a type as JSON without turning it into `Json` each time, give
it a `Show` and a `Read` made from its `Encoder` and `Decoder`:

```scala mdoc:compile-only
import io.circe.{Decoder, Encoder}

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.interop.circe.*

final case class User(name: String, age: Int)

implicit val userEncoder: Encoder[User] = Encoder.forProduct2("name", "age")(u => (u.name, u.age))
implicit val userDecoder: Decoder[User] = Decoder.forProduct2("name", "age")(User.apply)

implicit val userShow: Show[User]       = showAsJson[User]
implicit val userRead: Read[Bulk, User] = readAsJson[User]

val save = set("user", User("Ada", 36))
val load = get[User]("user")
```

Only `Json` and the types you choose are written as JSON. Other values, such as
a `String` or an `Int`, are written and read as they are.

Values are written as compact JSON, without spaces or line breaks. If a stored
value isn't valid JSON, or doesn't match the `Decoder`, reading it gives an
error.
