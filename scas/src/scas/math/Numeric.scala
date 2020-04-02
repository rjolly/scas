package scas.math

trait Numeric[T] extends Ordering[T] {
  def (x: T) + (y: T): T
  def (x: T) - (y: T): T
  def (x: T) * (y: T): T
  def (x: T).unary_- : T
  def fromInt(x: Int): T
  def toInt(x: T): Int
  def toLong(x: T): Long

  def zero = fromInt(0)
  def one = fromInt(1)

  def abs(x: T) = if (x < zero) -x else x

  def signum(x: T) =
    if (x < zero) -1
    else if (x > zero) 1
    else 0
}

object Numeric {
  def apply[T : Numeric] = summon[Numeric[T]]
  trait ByteIsIntegral extends Numeric[Byte] with scala.math.Numeric.ByteIsIntegral
  given ByteIsIntegral as ByteIsIntegral with Ordering.ByteOrdering
  trait ShortIsIntegral extends Numeric[Short] with scala.math.Numeric.ShortIsIntegral
  given ShortIsIntegral as ShortIsIntegral with Ordering.ShortOrdering
  trait IntIsIntegral extends Numeric[Int] with scala.math.Numeric.IntIsIntegral
  given IntIsIntegral as IntIsIntegral with Ordering.IntOrdering
  trait LongIsIntegral extends Numeric[Long] with scala.math.Numeric.LongIsIntegral
  given LongIsIntegral as LongIsIntegral with Ordering.LongOrdering
}
