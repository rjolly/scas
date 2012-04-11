package scas.structure

trait Structure[T] extends Ordering[T] { outer =>
  def apply(x: T): T
  def apply(l: Long): T
  def random(numbits: Int)(implicit rnd: java.util.Random): T
  def toCode(x: T, precedence: Int) = x.toString
  trait Ops {
    val lhs: T
    def ><(rhs: T) = equiv(lhs, rhs)
    def <>(rhs: T) = !equiv(lhs, rhs)
    def toCode(precedence: Int) = outer.toCode(lhs, precedence)
  }
  implicit def mkOps(value: T): Ops = new Ops { val lhs = value }
}

object Structure {
  trait ExtraImplicits {
    implicit def infixOps[T: Structure](lhs: T) = implicitly[Structure[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Ordered[T] { this: T =>
    val factory: Structure[T]
    def compare(that: T) = factory.compare(this, that)
    def ><(that: T) = factory.equiv(this, that)
    def <>(that: T) = !factory.equiv(this, that)
    def toCode(precedence: Int) = factory.toCode(this, precedence)
    override def toString = toCode(0)
  }
}
