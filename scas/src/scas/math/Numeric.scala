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
  trait ByteIsIntegral extends Numeric[Byte]  {
    def (x: Byte) + (y: Byte) = (x + y).toByte
    def (x: Byte) - (y: Byte) = (x - y).toByte
    def (x: Byte) * (y: Byte) = (x * y).toByte
    def (x: Byte).unary_- = (-x).toByte
    def fromInt(x: Int) = x.toByte
    def toInt(x: Byte) = x.toInt
    def toLong(x: Byte) = x.toLong
    override def signum(x: Byte) = java.lang.Integer.signum(x)
  }
  given ByteIsIntegral as ByteIsIntegral with Ordering.ByteOrdering
  trait ShortIsIntegral extends Numeric[Short] {
    def (x: Short) + (y: Short) = (x + y).toShort
    def (x: Short) - (y: Short) = (x - y).toShort
    def (x: Short) * (y: Short) = (x * y).toShort
    def (x: Short).unary_- = (-x).toShort
    def fromInt(x: Int) = x.toShort
    def toInt(x: Short) = x.toInt
    def toLong(x: Short) = x.toLong
    override def signum(x: Short) = java.lang.Integer.signum(x)
  }
  given ShortIsIntegral as ShortIsIntegral with Ordering.ShortOrdering
  trait IntIsIntegral extends Numeric[Int] {
    def (x: Int) + (y: Int) = x + y
    def (x: Int) - (y: Int) = x - y
    def (x: Int) * (y: Int) = x * y
    def (x: Int).unary_- = -x
    def fromInt(x: Int) = x
    def toInt(x: Int) = x
    def toLong(x: Int) = x.toLong
    override def signum(x: Int) = java.lang.Integer.signum(x)
  }
  given IntIsIntegral as IntIsIntegral with Ordering.IntOrdering
  trait LongIsIntegral extends Numeric[Long] {
    def (x: Long) + (y: Long) = x + y
    def (x: Long) - (y: Long) = x - y
    def (x: Long) * (y: Long) = x * y
    def (x: Long).unary_- = -x
    def fromInt(x: Int) = x.toLong
    def toInt(x: Long) = x.toInt
    def toLong(x: Long) = x
    override def signum(x: Long) = java.lang.Long.signum(x)
  }
  given LongIsIntegral as LongIsIntegral with Ordering.LongOrdering
}
