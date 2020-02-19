package scas.structure

trait Ring[T] extends AbelianGroup[T] with Monoid[T]

object Ring with
  def apply[T: Ring] = summon[Ring[T]]
