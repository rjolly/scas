package scas.math

trait Ordering[T] extends scala.math.Ordering[T] with PartialOrdering[T] {
  extension (x: T) {
    override def <=(y: T) = compare(x, y) <= 0
    override def >=(y: T) = compare(x, y) >= 0
    override def < (y: T) = compare(x, y) < 0
    override def > (y: T) = compare(x, y) > 0
  }
}

object Ordering {
  trait ByteOrdering extends Ordering[Byte] {
    def compare(x: Byte, y: Byte) = java.lang.Byte.compare(x, y)
    extension (x: Byte) {
      override def < (y: Byte) = x < y
      override def > (y: Byte) = x > y
    }
  }
  given Byte: ByteOrdering with {}
  trait ShortOrdering extends Ordering[Short] {
    def compare(x: Short, y: Short) = java.lang.Short.compare(x, y)
    extension (x: Short) {
      override def < (y: Short) = x < y
      override def > (y: Short) = x > y
    }
  }
  given Short: ShortOrdering with {}
  trait IntOrdering extends Ordering[Int] {
    def compare(x: Int, y: Int) = java.lang.Integer.compare(x, y)
    extension (x: Int) {
      override def < (y: Int) = x < y
      override def > (y: Int) = x > y
    }
  }
  given Int: IntOrdering with {}
  trait LongOrdering extends Ordering[Long] {
    def compare(x: Long, y: Long) = java.lang.Long.compare(x, y)
    extension (x: Long) {
      override def < (y: Long) = x < y
      override def > (y: Long) = x > y
    }
  }
  given Long: LongOrdering with {}
}
