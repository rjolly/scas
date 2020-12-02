package scas.math

trait Numeric[T] extends Ordering[T] {
  extension (x: T) {
    def + (y: T): T
    def - (y: T): T
    def * (y: T): T
    def unary_- : T
  }
  def fromInt(x: Int): T
  extension (x: T) {
    def toInt: Int
    def toLong: Long
  }

  def zero = fromInt(0)
  def one = fromInt(1)

  def abs(x: T) = if (x.signum < 0) -x else x

  extension (x: T) def signum =
    if (x < zero) -1
    else if (x > zero) 1
    else 0
}

object Numeric {
  def apply[T : Numeric] = summon[Numeric[T]]
  trait ByteIsIntegral extends Numeric[Byte]  {
    extension (x: Byte) {
      def + (y: Byte) = (x + y).toByte
      def - (y: Byte) = (x - y).toByte
      def * (y: Byte) = (x * y).toByte
      def unary_- = (-x).toByte
    }
    def fromInt(x: Int) = x.toByte
    extension (x: Byte) {
      def toInt = x.toInt
      def toLong = x.toLong
      override def signum = java.lang.Integer.signum(x)
    }
  }
  given ByteIsIntegral: ByteIsIntegral with Ordering.ByteOrdering with {}
  trait ShortIsIntegral extends Numeric[Short] {
    extension (x: Short) {
      def + (y: Short) = (x + y).toShort
      def - (y: Short) = (x - y).toShort
      def * (y: Short) = (x * y).toShort
      def unary_- = (-x).toShort
    }
    def fromInt(x: Int) = x.toShort
    extension (x: Short) {
      def toInt = x.toInt
      def toLong = x.toLong
      override def signum = java.lang.Integer.signum(x)
    }
  }
  given ShortIsIntegral: ShortIsIntegral with Ordering.ShortOrdering with {}
  trait IntIsIntegral extends Numeric[Int] {
    extension (x: Int) {
      def + (y: Int) = x + y
      def - (y: Int) = x - y
      def * (y: Int) = x * y
      def unary_- = -x
    }
    def fromInt(x: Int) = x
    extension (x: Int) {
      def toInt = x
      def toLong = x.toLong
      override def signum = java.lang.Integer.signum(x)
    }
  }
  given IntIsIntegral: IntIsIntegral with Ordering.IntOrdering with {}
  trait LongIsIntegral extends Numeric[Long] {
    extension (x: Long) {
      def + (y: Long) = x + y
      def - (y: Long) = x - y
      def * (y: Long) = x * y
      def unary_- = -x
    }
    def fromInt(x: Int) = x.toLong
    extension (x: Long) {
      def toInt = x.toInt
      def toLong = x
      override def signum = java.lang.Long.signum(x)
    }
  }
  given LongIsIntegral: LongIsIntegral with Ordering.LongOrdering with {}
}
