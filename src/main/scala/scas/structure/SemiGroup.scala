package scas.structure

trait SemiGroup[T] extends Structure[T] { outer =>
  def times(x: T, y: T): T
  override implicit def mkOps(value: T): SemiGroup.Ops[T] = new SemiGroup.Ops[T] {
    val lhs = value
    val factory = outer
  }
}

object SemiGroup {
  trait ExtraImplicits {
    implicit def infixSemiGroupOps[T: SemiGroup](lhs: T) = implicitly[SemiGroup[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Structure.Element[T] with Ops[T] { this: T =>
  }
  trait Ops[T] extends Structure.Ops[T] {
    val factory: SemiGroup[T]
    def *(rhs: T) = factory.times(lhs, rhs)
  }
}
