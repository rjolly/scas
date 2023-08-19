package scas.structure

trait Ring[T] extends AbelianGroup[T] with Monoid[T] {
  def characteristic: scas.base.BigInteger
  def apply(n: Long): T
}

object Ring {
  trait Ops[T] extends AbelianGroup.Ops[T] with Monoid.Ops[T] { this: Ring[T] =>
  }
}
