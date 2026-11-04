# JSON with circe

`laserdisc-circe` stores values as JSON and reads them back, using
[circe](https://circe.github.io/circe/).

```scala
libraryDependencies += "io.laserdisc" %%% "laserdisc-circe" % "@VERSION@"
```

Import `laserdisc.interop.circe.*`. Any type with a circe `Encoder` can then be
written, and any type with a `Decoder` can be read:

```scala mdoc:compile-only
import io.circe.{Decoder, Encoder}

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.interop.circe.*

final case class User(name: String, age: Int)

implicit val userEncoder: Encoder[User] = Encoder.forProduct2("name", "age")(u => (u.name, u.age))
implicit val userDecoder: Decoder[User] = Decoder.forProduct2("name", "age")(User.apply)

val save = set("user", User("Ada", 36))
val load = get[User]("user")
```

Values are written as compact JSON, without spaces or line breaks. If a stored
value isn't valid JSON, or doesn't match the `Decoder`, reading it gives an
error.
