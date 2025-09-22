package scas.math

import scas.util.{Conversion, unary_~}

trait Ordering[T] extends scala.math.Ordering[T] with PartialOrdering[T]

object Ordering {
  def by[T, S : scala.math.Ordering as ord](f: T => S): Ordering[T] = new Ordering[T] {
    def compare(x: T, y: T) = ord.compare(f(x), f(y))
    override def lt(x: T, y: T): Boolean = ord.lt(f(x), f(y))
    override def gt(x: T, y: T): Boolean = ord.gt(f(x), f(y))
    override def gteq(x: T, y: T): Boolean = ord.gteq(f(x), f(y))
    override def lteq(x: T, y: T): Boolean = ord.lteq(f(x), f(y))
  }
  trait ByteOrdering extends Ordering[Byte] {
    def compare(x: Byte, y: Byte) = java.lang.Byte.compare(x, y)
    extension (x: Byte) {
      inline override def < [U: Conversion[Byte]](y: U) = x < (~y)
      inline override def > [U: Conversion[Byte]](y: U) = x > (~y)
    }
  }
  given Byte: ByteOrdering()
  trait ShortOrdering extends Ordering[Short] {
    def compare(x: Short, y: Short) = java.lang.Short.compare(x, y)
    extension (x: Short) {
      inline override def < [U: Conversion[Short]](y: U) = x < (~y)
      inline override def > [U: Conversion[Short]](y: U) = x > (~y)
    }
  }
  given Short: ShortOrdering()
  trait IntOrdering extends Ordering[Int] {
    def compare(x: Int, y: Int) = java.lang.Integer.compare(x, y)
    extension (x: Int) {
      inline override def < [U: Conversion[Int]](y: U) = x < (~y)
      inline override def > [U: Conversion[Int]](y: U) = x > (~y)
    }
  }
  given Int: IntOrdering()
  trait LongOrdering extends Ordering[Long] {
    def compare(x: Long, y: Long) = java.lang.Long.compare(x, y)
    extension (x: Long) {
      inline override def < [U: Conversion[Long]](y: U) = x < (~y)
      inline override def > [U: Conversion[Long]](y: U) = x > (~y)
    }
  }
  given Long: LongOrdering()
}
