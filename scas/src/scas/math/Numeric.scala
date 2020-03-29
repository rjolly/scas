package scas.math

trait Numeric[T] extends Ordering[T] with scala.math.Numeric[T] {
  def (x: T) + (y: T) = plus(x, y)
  def (x: T) - (y: T) = minus(x, y)
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
