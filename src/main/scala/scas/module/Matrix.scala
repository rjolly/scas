package scas.module

import scala.reflect.ClassTag
import scas.Variable
import scas.structure.{Field, Algebra, AlgebraOverRing}
import scas.Implicits.infixUFDOps
import Matrix.Element

trait Matrix[R] extends AbstractModule[Element[R], R] with Algebra[Element[R], R] {
  val size: Int
  val dimension = size * size
  def generators = (for (i <- 0 until dimension) yield generator(i)).grouped(size).toArray
  def times(x: Element[R], y: Element[R]) = apply((for (i <- 0 until dimension) yield x(i) * y(i)).toArray)
  override def toCode(x: Element[R], precedence: Int) = name match {
    case Some(name) => {
      var s = ring.zero.toCode(0)
      var n = 0
      var m = 0
      for (i <- 0 until size; j <- 0 until size) {
        val c = ring.abs(x(i, j))
        val g = ring.signum(x(i, j)) < 0
        if (!c.isZero) {
          val (t, u) = {
            if (c.isOne) (Variable(name, i, j).toString, 1)
            else (c.toCode(1) + "*" + Variable(name, i, j).toString, 2)
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
    case _ => "matrix(" + x.value.mkString(", ") + ")"
  }
  override def toString = ring.toString + "^" + size + "^2"
  def toMathML(x: Element[R]) = name match {
    case Some(name) => {
      var s = ring.zero.toMathML
      var n = 0
      for (i <- 0 until size; j <- 0 until size) {
        val c = ring.abs(x(i, j))
        val g = ring.signum(x(i, j)) < 0
        if (!c.isZero) {
          val t = {
            if (c.isOne) Variable(name, i, j).toMathML
            else <apply><times/>{c.toMathML}{Variable(name, i, j).toMathML}</apply>
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
    case _ => <matrix>{for (j <- 0 until size) yield <matrixrow>{for (i <- 0 until size) yield x(i, j).toMathML}</matrixrow>}</matrix>
  }
  def toMathML = <msup>{ring.toMathML}<msup><mn>{size}</mn><mn>2</mn></msup></msup>
  override def apply(value: Array[R]) = new Element(value)(this)
}

object Matrix {
  def apply[R](name: String, size: Int, ring: Field[R])(implicit m: ClassTag[Element[R]], cm: ClassTag[R]) = new MatrixImpl(size, Some(name), ring)
  def apply[R](size: Int, ring: Field[R])(implicit m: ClassTag[Element[R]], cm: ClassTag[R]) = new MatrixImpl(size, None, ring)

  class Element[R](val value: Array[R])(val factory: Matrix[R]) extends AbstractModule.Element[Element[R], R] with AlgebraOverRing.Element[Element[R], R] with ((Int, Int) => R) {
    def apply(m: Int, n: Int): R = apply(m * factory.size + n)
  }
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def ring2matrixScalar[S <% R, R: Matrix](value: S) = implicitly[Matrix[R]].scalar(value)
  }
}
