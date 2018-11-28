
object ciris {

  implicit val decodeHost: ConfigDecoder[String, laserdisc.Host] =
    ConfigDecoder.catchNonFatal("host")(s => laserdisc.Host.unsafeFrom(s))

}
