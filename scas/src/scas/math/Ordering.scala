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
  trait ByteOrdering extends Ordering[Byte] {
    def compare(x: Byte, y: Byte) = java.lang.Byte.compare(x, y)
    override def (x: Byte) < (y: Byte) = x < y
    override def (x: Byte) > (y: Byte) = x > y
  }
  given Byte as ByteOrdering
  trait ShortOrdering extends Ordering[Short] {
    def compare(x: Short, y: Short) = java.lang.Short.compare(x, y)
    override def (x: Short) < (y: Short) = x < y
    override def (x: Short) > (y: Short) = x > y
  }
  given Short as ShortOrdering
  trait IntOrdering extends Ordering[Int] {
    def compare(x: Int, y: Int) = java.lang.Integer.compare(x, y)
    override def (x: Int) < (y: Int) = x < y
    override def (x: Int) > (y: Int) = x > y
  }
  given Int as IntOrdering
  trait LongOrdering extends Ordering[Long] {
    def compare(x: Long, y: Long) = java.lang.Long.compare(x, y)
    override def (x: Long) < (y: Long) = x < y
    override def (x: Long) > (y: Long) = x > y
  }
  given Long as LongOrdering

  given ord2comp[T] as Conversion[Ordering[T], scala.math.Ordering[T]] = ord => new scala.math.Ordering[T] {
    def compare(x: T, y: T) = ord.compare(x, y)
  }
}
