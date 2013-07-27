package scas.structure

import scala.xml.Elem
import scas.MathObject
import spire.macros.Ops
import Structure.OpsImpl

trait Structure[@specialized(Int, Long) T] extends Equiv[T] {
  def convert(x: T): T
  def apply(l: Long): T
  def random(numbits: Int)(implicit rnd: java.util.Random): T
  def nequiv(x: T, y: T) = !equiv(x, y)
  def toCode(x: T, precedence: Int) = x.toString
  def toMathML(x: T): Elem
  def toMathML: Elem
  implicit def mkOps(lhs: T): Structure.Ops[T] = new OpsImpl(lhs)(this)
}

object Structure {
  trait ExtraImplicits {
    implicit def infixOps[T: Structure](lhs: T) = implicitly[Structure[T]].mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  trait Element[T <: Element[T]] extends MathObject { this: T =>
    val lhs = this
    val factory: Structure[T]
    def ><(rhs: T) = factory.equiv(lhs, rhs)
    def <>(rhs: T) = factory.nequiv(lhs, rhs)
    override def toString = toCode(0)
    def toCode(precedence: Int) = factory.toCode(lhs, precedence)
    def toMathML = factory.toMathML(lhs)
  }
  trait Ops[T] {
    def ><(rhs: T) = macro Ops.binop[T, Boolean]
    def <>(rhs: T) = macro Ops.binop[T, Boolean]
    def toCode(rhs: Int) = macro Ops.binop[Int, String]
    def toMathML() = macro Ops.unop[Elem]
  }
  class OpsImpl[T: Structure](lhs: T) extends Ops[T]
}
