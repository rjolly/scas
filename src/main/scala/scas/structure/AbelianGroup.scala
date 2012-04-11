package scas.structure

trait AbelianGroup[T] extends Structure[T] { outer =>
  def zero = apply(0)
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def negate(x: T) = zero - x
  def abs(x: T) = if (signum(x) < 0) -x else x
  def signum(x: T) = if (x < zero) -1 else if (x > zero) 1 else 0
  def isZero(x: T) = x >< zero
  trait Ops extends super.Ops {
    def isZero = outer.isZero(lhs)
    def +(rhs: T) = plus(lhs, rhs)
    def -(rhs: T) = minus(lhs, rhs)
    def unary_- = negate(lhs)
    def unary_+ = lhs
  }
  override implicit def mkOps(value: T): Ops = new Ops { val lhs = value }
}

object AbelianGroup {
  trait ExtraImplicits {
    implicit def infixAbelianGroupOps[T: AbelianGroup](lhs: T) = implicitly[AbelianGroup[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Structure.Element[T] { this: T =>
    val factory: AbelianGroup[T]
    def isZero = factory.isZero(this)
    def +(that: T) = factory.plus(this, that)
    def -(that: T) = factory.minus(this, that)
    def unary_- = factory.negate(this)
    def unary_+ = this
  }
}
