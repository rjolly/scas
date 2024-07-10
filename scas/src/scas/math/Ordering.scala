package scas.math

import scas.util.{Conversion, unary_~}

trait Ordering[T] extends scala.math.Ordering[T] with PartialOrdering[T]

object Ordering {
  trait ByteOrdering extends Ordering[Byte] {
    def compare(x: Byte, y: Byte) = java.lang.Byte.compare(x, y)
    extension (x: Byte) {
      inline override def < [U: Conversion[Byte]](y: U) = x < (~y)
      inline override def > [U: Conversion[Byte]](y: U) = x > (~y)
    }
  }
  given ByteOrdering as Byte
  trait ShortOrdering extends Ordering[Short] {
    def compare(x: Short, y: Short) = java.lang.Short.compare(x, y)
    extension (x: Short) {
      inline override def < [U: Conversion[Short]](y: U) = x < (~y)
      inline override def > [U: Conversion[Short]](y: U) = x > (~y)
    }
  }
  given ShortOrdering as Short
  trait IntOrdering extends Ordering[Int] {
    def compare(x: Int, y: Int) = java.lang.Integer.compare(x, y)
    extension (x: Int) {
      inline override def < [U: Conversion[Int]](y: U) = x < (~y)
      inline override def > [U: Conversion[Int]](y: U) = x > (~y)
    }
  }
  given IntOrdering as Int
  trait LongOrdering extends Ordering[Long] {
    def compare(x: Long, y: Long) = java.lang.Long.compare(x, y)
    extension (x: Long) {
      inline override def < [U: Conversion[Long]](y: U) = x < (~y)
      inline override def > [U: Conversion[Long]](y: U) = x > (~y)
    }
  }
  given LongOrdering as Long
}
