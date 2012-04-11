package scas.structure

trait NotQuiteGroup[T] extends Monoid[T] {
  override def pow(x: T, exp: java.math.BigInteger) = if (exp.signum() < 0) pow(inverse(x), exp.negate()) else super.pow(x, exp)
  def inverse(x: T): T
}
