package scas.structure

trait SemiGroup[T] extends Structure[T] {
  def times(x: T, y: T): T
  trait Ops extends super.Ops {
    def *(rhs: T) = times(lhs, rhs)
  }
  override implicit def mkOps(value: T): Ops = new Ops { val lhs = value }
}

object SemiGroup {
  trait ExtraImplicits {
    implicit def infixSemiGroupOps[T: SemiGroup](lhs: T) = implicitly[SemiGroup[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Structure.Element[T] { this: T =>
    val factory: SemiGroup[T]
    def *(that: T) = factory.times(this, that)
  }
}
