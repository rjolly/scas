package scas.structure

trait SemiGroup[T] extends Structure[T] with
  def (x: T) * (y: T): T
  def (x: T) *:(y: T) = x * y

object SemiGroup with
  def apply[T: SemiGroup] = summon[SemiGroup[T]]

  trait Ops[T] extends Structure.Ops[T] with
    override def factory: SemiGroup[T]
    given SemiGroup[T] = factory
    def * (y: T) = x * y

  class OpsImpl[T: SemiGroup](val x: T) extends Ops[T] with
    def factory = SemiGroup[T]
