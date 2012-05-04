package scas.structure

import scala.xml.Elem

trait Structure[T] extends Ordering[T] { outer =>
  def convert(x: T): T
  def apply(l: Long): T
  def random(numbits: Int)(implicit rnd: java.util.Random): T
  def toCode(x: T, precedence: Int) = x.toString
  def toMathML(x: T): Elem
  def toMathML: Elem
  implicit def mkOps(value: T): Structure.Ops[T] = new Structure.Ops[T] {
    val lhs = value
    val factory = outer
  }
}

object Structure {
  trait ExtraImplicits {
    implicit def infixOps[T: Structure](lhs: T) = implicitly[Structure[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends Ordered[T] with Ops[T] { this: T =>
    val lhs = this
    def compare(rhs: T) = factory.compare(lhs, rhs)
    override def toString = toCode(0)
  }
  trait Ops[T] {
    val lhs: T
    val factory: Structure[T]
    def ><(rhs: T) = factory.equiv(lhs, rhs)
    def <>(rhs: T) = !factory.equiv(lhs, rhs)
    def toCode(precedence: Int) = factory.toCode(lhs, precedence)
    def toMathML = factory.toMathML(lhs)
  }
}
