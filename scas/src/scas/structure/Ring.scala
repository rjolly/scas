package scas.structure

trait Ring[T] extends AbelianGroup[T] with Monoid[T] {
  def characteristic: scas.base.BigInteger
}
