package laserdisc
package protocol

import eu.timepit.refined.api.Refined

import scala.annotation.implicitNotFound

@implicitNotFound(
  """Implicit not found Show[${A}].

Try writing your own, for example:

implicit final val myShow: Show[${A}] = new Show[${A}] {
  override final def show(a: ${A}): String = ???
}
"""
) trait Show[A] {
  def show(a: A): String

  final def contramap[B](f: B => A): Show[B] = Show.instance(show _ compose f)
}

object Show extends ShowInstances {
  @inline final def apply[A](implicit instance: Show[A]): Show[A] = instance

  final def const[A](s: => String): Show[A] = new Show[A] {
    override def show(a: A): String = s
  }
  final def unsafeFromToString[A]: Show[A] = new Show[A] {
    override def show(a: A): String = a.toString
  }
  final def instance[A](f: A => String): Show[A] = new Show[A] {
    override def show(a: A): String = f(a)
  }
}

private[protocol] sealed trait ShowInstances {
  private[this] final val refinedDoubleCases: PartialFunction[Refined[Double, _], String] = {
    case d if d.value == Double.NegativeInfinity => "-inf"
    case d if d.value == Double.PositiveInfinity => "+inf"
    case d                                       => d.value.toString
  }

  implicit final val doubleShow: Show[Double] = Show.unsafeFromToString
  implicit final val intShow: Show[Int]       = Show.unsafeFromToString
  implicit final val longShow: Show[Long]     = Show.unsafeFromToString
  implicit final val stringShow: Show[String] = Show.instance(identity)
  implicit final val symbolShow: Show[Symbol] = Show.instance(_.name)

  implicit final val connectionNameShow: Show[ConnectionName] = Show.unsafeFromToString
  implicit final val dbIndexShow: Show[DbIndex]               = Show.unsafeFromToString
  implicit final val globPatternShow: Show[GlobPattern]       = Show.unsafeFromToString
  implicit final val hostShow: Show[Host]                     = Show.unsafeFromToString
  implicit final val indexShow: Show[Index]                   = Show.unsafeFromToString
  implicit final val keyShow: Show[Key]                       = Show.unsafeFromToString
  implicit final val latitudeShow: Show[Latitude]             = Show.unsafeFromToString
  implicit final val longitudeShow: Show[Longitude]           = Show.unsafeFromToString
  implicit final val nodeIdShow: Show[NodeId]                 = Show.unsafeFromToString
  implicit final val nonNegDoubleShow: Show[NonNegDouble]     = Show.instance(refinedDoubleCases)
  implicit final val nonNegIntShow: Show[NonNegInt]           = Show.unsafeFromToString
  implicit final val nonNegLongShow: Show[NonNegLong]         = Show.unsafeFromToString
  implicit final val nonZeroDoubleShow: Show[NonZeroDouble]   = Show.instance(refinedDoubleCases)
  implicit final val nonZeroIntShow: Show[NonZeroInt]         = Show.unsafeFromToString
  implicit final val nonZeroLongShow: Show[NonZeroLong]       = Show.unsafeFromToString
  implicit final val portShow: Show[Port]                     = Show.unsafeFromToString
  implicit final val posIntShow: Show[PosInt]                 = Show.unsafeFromToString
  implicit final val posLongShow: Show[PosLong]               = Show.unsafeFromToString
  implicit final val rangeOffsetShow: Show[RangeOffset]       = Show.unsafeFromToString
  implicit final val slotShow: Show[Slot]                     = Show.unsafeFromToString
  implicit final val stringLengthShow: Show[StringLength]     = Show.unsafeFromToString
  implicit final val validDoubleShow: Show[ValidDouble]       = Show.instance(refinedDoubleCases)
}
