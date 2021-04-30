package scas.module

import scala.reflect.ClassTag
import scas.structure.Ring
import scas.structure.Module
import scas.util.ClassTagArray

class ArrayModule[R : ClassTag : ClassTagArray](using ring: Ring[R])(val dimension: Int) extends Module[Array[R], R] {
  def generator(n: Int) = (for (i <- 0 until dimension) yield if (i == n) ring.one else ring.zero).toArray
  def generators = (for (i <- 0 until dimension) yield generator(i)).toArray
  def apply(x: Array[R]) = x
  override def convert(x: Array[R]) = (for (i <- 0 until dimension) yield if (i < x.length) ring.convert(x(i)) else ring.zero).toArray
  def equiv(x: Array[R], y: Array[R]): Boolean = {
    for (i <- 0 until dimension) {
      if (x(i) <> y(i)) return false
    }
    true
  }
  extension (x: R) def *%(y: Array[R]) = (for (i <- 0 until dimension) yield x * y(i)).toArray
  extension (x: Array[R]) {
    def %* (y: R) = (for (i <- 0 until dimension) yield x(i) * y).toArray
    def add(y: Array[R]) = (for (i <- 0 until dimension) yield x(i) + y(i)).toArray
    def subtract(y: Array[R]) = (for (i <- 0 until dimension) yield x(i) - y(i)).toArray
    def signum = x.foldLeft(0)((l, r) => if (l == 0) r.signum else l)
    def toCode(level: Level) = "Array(" + x.map(_.show).mkString(", ") + ")"
    def toMathML = s"<vector>${x.map(_.toMathML).mkString}</vector>"
  }
  def zero = (for (i <- 0 until dimension) yield ring.zero).toArray
  override def toString = s"$ring.module($dimension)"
  def toMathML = s"<msup>${ring.toMathML}<cn>${dimension}</cn></msup>"

  extension (ring: Ring[R]) def module(n: Int) = {
    assert (n == dimension)
    this
  }
}
