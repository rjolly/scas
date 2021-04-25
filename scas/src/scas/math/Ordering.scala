package scas.math

trait Ordering[T] extends scala.math.Ordering[T] with PartialOrdering[T]

object Ordering {
  trait ByteOrdering extends Ordering[Byte] {
    def compare(x: Byte, y: Byte) = java.lang.Byte.compare(x, y)
    extension (x: Byte) {
      inline override def < (y: Byte) = x < y
      inline override def > (y: Byte) = x > y
    }
  }
  given Byte: ByteOrdering with {}
  trait ShortOrdering extends Ordering[Short] {
    def compare(x: Short, y: Short) = java.lang.Short.compare(x, y)
    extension (x: Short) {
      inline override def < (y: Short) = x < y
      inline override def > (y: Short) = x > y
    }
  }
  given Short: ShortOrdering with {}
  trait IntOrdering extends Ordering[Int] {
    def compare(x: Int, y: Int) = java.lang.Integer.compare(x, y)
    extension (x: Int) {
      inline override def < (y: Int) = x < y
      inline override def > (y: Int) = x > y
    }
  }
  given Int: IntOrdering with {}
  trait LongOrdering extends Ordering[Long] {
    def compare(x: Long, y: Long) = java.lang.Long.compare(x, y)
    extension (x: Long) {
      inline override def < (y: Long) = x < y
      inline override def > (y: Long) = x > y
    }
  }
  given Long: LongOrdering with {}
}
