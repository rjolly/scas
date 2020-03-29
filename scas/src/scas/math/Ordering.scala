package scas.math

trait Ordering[T] extends PartialOrdering[T] with scala.math.Ordering[T] {
  def (x: T) < (y: T) = lt(x, y)
  def (x: T) <=(y: T) = lteq(x, y)
  def (x: T) > (y: T) = gt(x, y)
  def (x: T) >=(y: T) = gteq(x, y)
}

object Ordering {
  trait ByteOrdering extends Ordering[Byte] with scala.math.Ordering.ByteOrdering
  given Byte as ByteOrdering
  trait ShortOrdering extends Ordering[Short] with scala.math.Ordering.ShortOrdering
  given Short as ShortOrdering
  trait IntOrdering extends Ordering[Int] with scala.math.Ordering.IntOrdering
  given Int as IntOrdering
  trait LongOrdering extends Ordering[Long] with scala.math.Ordering.LongOrdering
  given Long as LongOrdering
}
