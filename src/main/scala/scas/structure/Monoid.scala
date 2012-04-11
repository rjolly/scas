package scas.structure

trait Monoid[T] extends SemiGroup[T] { outer =>
  def one = apply(1)
  def pow(x: T, exp: java.math.BigInteger) = {
    assert (exp.intValue() >= 0)
    (one /: (1 to exp.intValue())) { (l, r) => l * x }
  }
  def isUnit(x: T): Boolean
  def isOne(x: T) = x >< one
  trait Ops extends super.Ops {
    def isUnit = outer.isUnit(lhs)
    def isOne = outer.isOne(lhs)
  }
  override implicit def mkOps(value: T): Ops = new Ops { val lhs = value }
}

object Monoid {
  trait ExtraImplicits {
    implicit def infixMonoidOps[T: Monoid](lhs: T) = implicitly[Monoid[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends SemiGroup.Element[T] { this: T =>
    val factory: Monoid[T]
    def isUnit = factory.isUnit(this)
    def isOne = factory.isOne(this)
  }
}
