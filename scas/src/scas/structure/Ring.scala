package scas.structure

trait Ring[T] extends AbelianGroup[T] with Monoid[T] with
  def characteristic: scas.BigInteger

object Ring with
  def apply[T: Ring] = summon[Ring[T]]

  trait Ops[T] extends AbelianGroup.Ops[T] with SemiGroup.Ops[T] with
    override def factory: Ring[T]
    given Ring[T] = factory

  class OpsImpl[T: Ring](val x: T) extends Ops[T] with
    def factory = Ring[T]
