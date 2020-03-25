package scas.structure

trait Ring[T] extends AbelianGroup[T] with Monoid[T] {
  def characteristic: scas.BigInteger
}

object Ring {
  def apply[T : Ring] = summon[Ring[T]]
}
