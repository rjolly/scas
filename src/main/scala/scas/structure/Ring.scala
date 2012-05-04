package scas.structure

trait Ring[T] extends AbelianGroup[T] with Monoid[T] { outer =>
  def characteristic: java.math.BigInteger
  override implicit def mkOps(value: T): Ring.Ops[T] = new Ring.Ops[T] {
    val lhs = value
    val factory = outer
  }
}

object Ring {
  trait ExtraImplicits {
    implicit def infixRingOps[T: Ring](lhs: T) = implicitly[Ring[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends AbelianGroup.Element[T] with Monoid.Element[T] with Ops[T] { this: T =>
  }
  trait Ops[T] extends AbelianGroup.Ops[T] with Monoid.Ops[T] {
    val factory: Ring[T]
  }
}
