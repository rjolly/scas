package scas.structure

trait Ring[T] extends AbelianGroup[T] with Monoid[T] with
  def characteristic: scas.BigInteger
