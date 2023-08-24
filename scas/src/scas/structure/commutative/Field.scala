package scas.structure.commutative

trait Field[T] extends Field.Impl[T] with scas.structure.Field[T] with EuclidianDomain[T]

object Field {
  trait Impl[T] extends scas.structure.Field.Impl[T] with EuclidianDomain.Impl[T] {
    override def gcd(x: T, y: T) = if (y.isZero) x else y
    extension (x: T) {
      override def divide(y: T) = super.divide(x)(y)
      override def remainder(y: T) = zero
      override def divideAndRemainder(y: T) = (x / y, x % y)
    }
  }
  def apply[T : Impl] = summon[Impl[T]]
}
