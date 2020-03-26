package scas.math

trait Ordering[T] extends PartialOrdering[T] {
  def compare(x: T, y: T): Int
  override def (x: T) <=(y: T) = compare(x, y) <= 0
  override def (x: T) >=(y: T) = compare(x, y) >= 0
  override def (x: T) < (y: T) = compare(x, y) < 0
  override def (x: T) > (y: T) = compare(x, y) > 0
  override def equiv(x: T, y: T) = compare(x, y) == 0
  def max(x: T, y: T) = if (x >= y) x else y
  def min(x: T, y: T) = if (x <= y) x else y
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
