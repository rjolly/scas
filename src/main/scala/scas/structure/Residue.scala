package scas.structure

trait Residue[T] extends Ring[T] {
  abstract override def convert(x: T) = reduce(super.convert(x))
  abstract override def apply(l: Long) = reduce(super.apply(l))
  def reduce(x: T): T
  abstract override def random(numbits: Int)(implicit rnd: java.util.Random) = reduce(super.random(numbits))
  override def pow(x: T, exp: java.math.BigInteger) = reduce(super.pow(x, exp))
  override def negate(x: T) = reduce(super.negate(x))
  override def abs(x: T) = reduce(super.abs(x))
  abstract override def plus(x: T, y: T) = reduce(super.plus(x, y))
  abstract override def minus(x: T, y: T) = reduce(super.minus(x, y))
  abstract override def times(x: T, y: T) = reduce(super.times(x, y))
}
