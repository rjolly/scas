package scas.module

import scas.structure.{Ring, Module}
import scas.util.ToFrags

class ArrayModule[R : ClassTag : ClassTagArray](using ring: Ring[R])(val dimension: Int) extends Module[Array[R], R] {
  given ArrayModule[R] = this
  def generator(n: Int) = (for (i <- 0 until dimension) yield if (i == n) ring.one else ring.zero).toArray
  def generators = (for (i <- 0 until dimension) yield generator(i)).toArray
  override def apply(x: Array[R]) = (for (i <- 0 until dimension) yield if (i < x.length) ring(x(i)) else ring.zero).toArray
  def equiv(x: Array[R], y: Array[R]): Boolean = {
    for (i <- 0 until dimension) {
      if (x(i) <> y(i)) return false
    }
    true
  }
  def apply[S : ToFrags[R]](x: S) = x.toFrags
  extension (x: R) def *%(y: Array[R]) = (for (i <- 0 until dimension) yield x * y(i)).toArray
  extension (x: Array[R]) {
    def %* (y: R) = (for (i <- 0 until dimension) yield x(i) * y).toArray
    def add(y: Array[R]) = (for (i <- 0 until dimension) yield x(i) + y(i)).toArray
    def subtract(y: Array[R]) = (for (i <- 0 until dimension) yield x(i) - y(i)).toArray
    def signum = x.foldLeft(0)((l, r) => if (l == 0) r.signum else l)
    def toCode(level: Level) = "Array(" + x.map(_.show).mkString(", ") + ")"
    def toMathML = s"<vector>${x.map(_.toMathML)}</vector>"
  }
  def zero = (for (i <- 0 until dimension) yield ring.zero).toArray
  override def toString = s"$ring\\$dimension"
  def toMathML = s"<apply><power/>${ring.toMathML}<cn>${dimension}</cn></apply>"

  extension (ring: Ring[R]) def \ (n: Int) = this
}
