package scas.structure

trait Ring[T] extends AbelianGroup[T] with Monoid[T] {
  def characteristic: scas.base.BigInteger
  def apply(n: Long): T
}

object Ring {
  trait Ops[T: Ring] extends AbelianGroup.Ops[T] with SemiGroup.Ops[T]
}
