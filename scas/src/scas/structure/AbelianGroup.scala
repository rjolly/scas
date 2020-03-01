package scas.structure

trait AbelianGroup[T] extends Structure[T] with
  def (x: T) + (y: T): T
  def (x: T) +:(y: T) = x + y
  def (x: T) - (y: T): T
  def (x: T) -:(y: T) = x - y
  def (x: T).unary_- = zero - x
  def abs(x: T) = if (signum(x) < 0) -x else x
  def signum(x: T): Int
  def zero: T

object AbelianGroup with
  def apply[T: AbelianGroup] = summon[AbelianGroup[T]]

  trait Ops[T] extends Structure.Ops[T] with
    override def factory: AbelianGroup[T]
    given AbelianGroup[T] = factory
    def + (y: T) = x + y
    def - (y: T) = x - y

  class OpsImpl[T: AbelianGroup](val x: T) extends Ops[T] with
    def factory = AbelianGroup[T]
