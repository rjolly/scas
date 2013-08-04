package scas.module

import scala.reflect.ClassTag
import scas.Variable
import scas.structure.{Ring, Field}
import scas.Implicits.infixRingOps
import Module.Element

trait Module[R] extends AbstractModule[Element[R], R] {
  override def toCode(x: Element[R], precedence: Int) = name match {
    case Some(name) => {
      var s = ring.zero.toCode(0)
      var n = 0
      var m = 0
      for (i <- 0 until dimension) {
        val c = ring.abs(x(i))
        val g = ring.signum(x(i)) < 0
        if (!c.isZero) {
          val (t, u) = {
            if (c.isOne) (Variable(name, i).toString, 1)
            else (c.toCode(1) + "*" + Variable(name, i).toString, 2)
          }
          s = {
            if (n == 0) {
              if (g) "-" + t else t
            } else {
              if (g) s + "-" + t else s + "+" + t
            }
          }
          m = if (g) u + 1 else u
          n += 1
        }
      }
      val fenced = {
        if (n == 0) false
        else if (n == 1) {
          if (m == 1) false
          else precedence > 1
        } else precedence > 0
      }
      if (fenced) "(" + s + ")" else s
    }
    case _ => "vector(" + x.value.mkString(", ") + ")"
  }
  override def toString = ring.toString + "^" + dimension
  def toMathML(x: Element[R]) = name match {
    case Some(name) => {
      var s = ring.zero.toMathML
      var n = 0
      for (i <- 0 until dimension) {
        val c = ring.abs(x(i))
        val g = ring.signum(x(i)) < 0
        if (!c.isZero) {
          val t = {
            if (c.isOne) Variable(name, i).toMathML
            else <apply><times/>{c.toMathML}{Variable(name, i).toMathML}</apply>
          }
          s = {
            if (n == 0) {
              if (g) <apply><minus/>{t}</apply> else t
            } else {
              if (g) <apply><minus/>{s}{t}</apply> else <apply><plus/>{s}{t}</apply>
            }
          }
          n += 1
        }
      }
      s
    }
    case _ => <vector>{x.value.map(_.toMathML)}</vector>
  }
  def toMathML = <msup>{ring.toMathML}<mn>{dimension}</mn></msup>
  def apply(value: Array[R]) = new Element(value)(this)
}

object Module {
  def apply[R](name: String, dimension: Int, ring: Ring[R])(implicit m: ClassTag[Element[R]], cm: ClassTag[R]) = new ModuleImpl(dimension, Some(name), ring)
  def apply[R](dimension: Int, ring: Ring[R])(implicit m: ClassTag[Element[R]], cm: ClassTag[R]) = new ModuleImpl(dimension, None, ring)
  def apply[R](name: String, dimension: Int, ring: Field[R])(implicit m: ClassTag[Element[R]], cm: ClassTag[R]) = new Vector(dimension, Some(name), ring)
  def apply[R](dimension: Int, ring: Field[R])(implicit m: ClassTag[Element[R]], cm: ClassTag[R]) = new Vector(dimension, None, ring)

  class Element[R](val value: Array[R])(val factory: Module[R]) extends AbstractModule.Element[Element[R], R]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def ring2scalar[S <% R, R: Module](value: S) = implicitly[Module[R]].scalar(value)
  }
}
