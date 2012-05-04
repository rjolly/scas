package scas.structure

trait AbelianGroup[T] extends Structure[T] { outer =>
  def zero = apply(0)
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def negate(x: T) = zero - x
  def abs(x: T) = if (signum(x) < 0) -x else x
  def signum(x: T) = if (x < zero) -1 else if (x > zero) 1 else 0
  def isZero(x: T) = x >< zero
  override implicit def mkOps(value: T): AbelianGroup.Ops[T] = new AbelianGroup.Ops[T] {
    val lhs = value
    val factory = outer
  }
}

object AbelianGroup {
  trait ExtraImplicits {
    implicit def infixAbelianGroupOps[T: AbelianGroup](lhs: T) = implicitly[AbelianGroup[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Structure.Element[T] with Ops[T] { this: T =>
  }
  trait Ops[T] extends Structure.Ops[T] {
    val factory: AbelianGroup[T]
    def isZero = factory.isZero(lhs)
    def +(rhs: T) = factory.plus(lhs, rhs)
    def -(rhs: T) = factory.minus(lhs, rhs)
    def unary_- = factory.negate(lhs)
    def unary_+ = lhs
  }
}
