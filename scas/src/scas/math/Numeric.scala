package scas.math

trait Numeric[T] extends Ordering[T] {
  def (x: T) + (y: T): T
  def (x: T) - (y: T): T
  def (x: T) * (y: T): T
  def (x: T).unary_- : T
  def fromInt(x: Int): T
  def (x: T).toInt: Int
  def (x: T).toLong: Long

  def zero = fromInt(0)
  def one = fromInt(1)

  def abs(x: T) = if (x.signum < 0) -x else x

  def (x: T).signum =
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
    def (x: Byte).toInt = x.toInt
    def (x: Byte).toLong = x.toLong
    override def (x: Byte).signum = java.lang.Integer.signum(x)
  }
  given ByteIsIntegral as ByteIsIntegral with Ordering.ByteOrdering
  trait ShortIsIntegral extends Numeric[Short] {
    def (x: Short) + (y: Short) = (x + y).toShort
    def (x: Short) - (y: Short) = (x - y).toShort
    def (x: Short) * (y: Short) = (x * y).toShort
    def (x: Short).unary_- = (-x).toShort
    def fromInt(x: Int) = x.toShort
    def (x: Short).toInt = x.toInt
    def (x: Short).toLong = x.toLong
    override def (x: Short).signum = java.lang.Integer.signum(x)
  }
  given ShortIsIntegral as ShortIsIntegral with Ordering.ShortOrdering
  trait IntIsIntegral extends Numeric[Int] {
    def (x: Int) + (y: Int) = x + y
    def (x: Int) - (y: Int) = x - y
    def (x: Int) * (y: Int) = x * y
    def (x: Int).unary_- = -x
    def fromInt(x: Int) = x
    def (x: Int).toInt = x
    def (x: Int).toLong = x.toLong
    override def (x: Int).signum = java.lang.Integer.signum(x)
  }
  given IntIsIntegral as IntIsIntegral with Ordering.IntOrdering
  trait LongIsIntegral extends Numeric[Long] {
    def (x: Long) + (y: Long) = x + y
    def (x: Long) - (y: Long) = x - y
    def (x: Long) * (y: Long) = x * y
    def (x: Long).unary_- = -x
    def fromInt(x: Int) = x.toLong
    def (x: Long).toInt = x.toInt
    def (x: Long).toLong = x
    override def (x: Long).signum = java.lang.Long.signum(x)
  }
  given LongIsIntegral as LongIsIntegral with Ordering.LongOrdering
}
