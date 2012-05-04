package scas.structure

trait Monoid[T] extends SemiGroup[T] { outer =>
  def one = apply(1)
  def pow(x: T, exp: java.math.BigInteger) = {
    assert (exp.intValue() >= 0)
    (one /: (1 to exp.intValue())) { (l, r) => l * x }
  }
  def isUnit(x: T): Boolean
  def isOne(x: T) = x >< one
  override implicit def mkOps(value: T): Monoid.Ops[T] = new Monoid.Ops[T] {
    val lhs = value
    val factory = outer
  }
}

object Monoid {
  trait ExtraImplicits {
    implicit def infixMonoidOps[T: Monoid](lhs: T) = implicitly[Monoid[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends SemiGroup.Element[T] with Ops[T] { this: T =>
  }
  trait Ops[T] extends SemiGroup.Ops[T] {
    val factory: Monoid[T]
    def isUnit = factory.isUnit(lhs)
    def isOne = factory.isOne(lhs)
  }
}
