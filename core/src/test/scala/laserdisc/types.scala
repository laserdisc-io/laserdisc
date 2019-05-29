package laserdisc

final case class Foo(x: Int)
object Foo {
  implicit final val fooRead: Bulk ==> Foo = Read.instancePF { case Bulk(ToInt(i)) => Foo(i) }
}

final case class Bar(x: String)
