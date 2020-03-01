package scas.structure

trait Ring[T] extends AbelianGroup[T] with Monoid[T] with
  def characteristic: scas.BigInteger

object Ring with
  def apply[T: Ring] = summon[Ring[T]]
