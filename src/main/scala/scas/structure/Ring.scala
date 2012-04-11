package scas.structure

trait Ring[T] extends AbelianGroup[T] with Monoid[T] {
  def characteristic: java.math.BigInteger
  trait Ops extends super[AbelianGroup].Ops with super[Monoid].Ops
  override implicit def mkOps(value: T): Ops = new Ops { val lhs = value }
}

object Ring {
  trait ExtraImplicits {
    implicit def infixRingOps[T: Ring](lhs: T) = implicitly[Ring[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends AbelianGroup.Element[T] with Monoid.Element[T] { this: T =>
    val factory: Ring[T]
  }
}
