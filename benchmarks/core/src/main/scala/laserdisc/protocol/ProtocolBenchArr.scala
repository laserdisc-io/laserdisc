package laserdisc
package protocol

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import org.openjdk.jmh.infra.Blackhole
import shapeless._

@State(Scope.Benchmark)
class ProtocolBenchArr {
  final case class A(s: List[String])
  implicit val ev1: Arr ==> A =
    Read.infallible { arr =>
      A(arr.elements.map {
        case Bulk(s) => s
        case _ => ""
      })
    }

  private final def protocol = Protocol("CUSTOM", _: String :: HNil).as[Arr, Seq[Long]]
  private final def protocolArr = Protocol("CUSTOM", _: String :: HNil).as[Arr, Seq[A]]
  private final def protocolWithNull = Protocol("CUSTOM", _: String :: HNil).as[Arr, Seq[Option[Long]]]
  private final def protocolArrWithNull = Protocol("CUSTOM", _: String :: HNil).as[Arr, Seq[Option[A]]]
  private final def protocolPairs = Protocol("CUSTOM", _: String :: HNil).as[Arr, Seq[(String, Long)]]

  private final val request  = "id" :: HNil

  private final val response1 = Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"))
  private final val response2 = Arr(Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20"), Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20"))
  private final val response3 = Arr(
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"))
  )
  private final val response4 = Arr(
    Arr(Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20"), Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20")),
    Arr(Bulk("2"), Bulk("1"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk, Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), NullBulk, Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), NullBulk, Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20")),
    Arr(Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20"), Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk, Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20")),
    Arr(Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20"), Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk, Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk),
    Arr(Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20"), Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), NullBulk, Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), NullBulk, Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk, Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk),
    Arr(Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20"), Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk, Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20"), Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk, Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20"), Bulk("1"), Bulk("2"), Bulk("3"), Bulk("4"), Bulk("5"), Bulk("6"), Bulk("7"), Bulk("8"), Bulk("9"), Bulk("10"), Bulk("11"), Bulk("12"), Bulk("13"), Bulk("14"), Bulk("15"), Bulk("16"), Bulk("17"), Bulk("18"), Bulk("19"), Bulk("20")),
    Arr(Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20"), Bulk("1"), NullBulk, Bulk("3"), NullBulk, Bulk("5"), Bulk("6"), NullBulk, NullBulk, Bulk("9"), NullBulk, Bulk("11"), Bulk("12"), NullBulk, Bulk("14"), NullBulk, Bulk("16"), Bulk("17"), Bulk("18"), NullBulk, Bulk("20")),
    Arr(Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk, Bulk("1"), Bulk("2"), Bulk("3"), NullBulk, Bulk("5"), NullBulk, Bulk("7"), NullBulk, Bulk("9"), NullBulk, NullBulk, NullBulk, NullBulk, NullBulk, Bulk("15"), Bulk("16"), NullBulk, Bulk("18"), Bulk("19"), NullBulk)
  )
  private final val response5 = Arr(Bulk("abcd-1"), Bulk("1"), Bulk("abcd-2"), Bulk("2"), Bulk("abcd-3"), Bulk("3"), Bulk("abcd-4"), Bulk("4"), Bulk("abcd-5"), Bulk("5"), Bulk("abcd-6"), Bulk("6"), Bulk("abcd-7"), Bulk("7"), Bulk("abcd-8"), Bulk("8"), Bulk("abcd-9"), Bulk("9"), Bulk("abcd-10"), Bulk("10"), Bulk("abcd-11"), Bulk("11"), Bulk("abcd-12"), Bulk("12"), Bulk("abcd-13"), Bulk("13"), Bulk("abcd-14"), Bulk("14"), Bulk("abcd-15"), Bulk("15"), Bulk("abcd-16"), Bulk("16"), Bulk("abcd-17"), Bulk("17"), Bulk("abcd-18"), Bulk("18"), Bulk("abcd-19"), Bulk("19"), Bulk("abcd-20"), Bulk("20"), Bulk("abcd-1"), Bulk("1"), Bulk("abcd-2"), Bulk("2"), Bulk("abcd-3"), Bulk("3"), Bulk("abcd-4"), Bulk("4"), Bulk("abcd-5"), Bulk("5"), Bulk("abcd-6"), Bulk("6"), Bulk("abcd-7"), Bulk("7"), Bulk("abcd-8"), Bulk("8"), Bulk("abcd-9"), Bulk("9"), Bulk("abcd-10"), Bulk("10"), Bulk("abcd-11"), Bulk("11"), Bulk("abcd-12"), Bulk("12"), Bulk("abcd-13"), Bulk("13"), Bulk("abcd-14"), Bulk("14"), Bulk("abcd-15"), Bulk("15"), Bulk("abcd-16"), Bulk("16"), Bulk("abcd-17"), Bulk("17"), Bulk("abcd-18"), Bulk("18"), Bulk("abcd-19"), Bulk("19"), Bulk("abcd-20"), Bulk("20"))

  @Benchmark def decodeArrBaseline(bh: Blackhole) = {
    val decoded = protocol(request).decode(Arr(Nil))
    bh.consume(decoded)
  }

  @Benchmark def decodeArrOfBulk(bh: Blackhole) = {
    val decoded = protocol(request).decode(response1)
    bh.consume(decoded)
  }
  @Benchmark def decodeArrOfBulkWithNull(bh: Blackhole) = {
    val decoded = protocolWithNull(request).decode(response2)
    bh.consume(decoded)
  }
  @Benchmark def decodeArrOfArrOfBulk(bh: Blackhole) = {
    val decoded = protocol(request).decode(response3)
    bh.consume(decoded)
  }
  @Benchmark def decodeArrOfArrOfBulkWithNull(bh: Blackhole) = {
    val decoded = protocolWithNull(request).decode(response4)
    bh.consume(decoded)
  }
  @Benchmark def decodeArrOfPairs(bh: Blackhole) = {
    val decoded = protocolPairs(request).decode(response5)
    bh.consume(decoded)
  }
}
