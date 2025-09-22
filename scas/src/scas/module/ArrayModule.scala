package scas.module

import scala.reflect.ClassTag
import scas.structure.{Ring, Module}
import scas.prettyprint.Show.given

class ArrayModule[R : ClassTag](using Ring[R])(val dimension: Int) extends Module[Array[R], R] {
  given instance: ArrayModule[R] = this
  def apply(x: Array[R]) = x
  def generator(n: Int) = (for i <- 0 until dimension yield if i == n then ring.one else ring.zero).toArray
  def generators = (for i <- 0 until dimension yield generator(i)).toList
  def equiv(x: Array[R], y: Array[R]): Boolean = {
    var i = 0
    while i < dimension do {
      if x(i) <> y(i) then return false
      i += 1
    }
    true
  }
  extension (x: R) def multiplyLeft(y: Array[R]) = (for i <- 0 until dimension yield x * y(i)).toArray
  extension (x: Array[R]) {
    def multiplyRight(y: R) = (for i <- 0 until dimension yield x(i) * y).toArray
    def add(y: Array[R]) = (for i <- 0 until dimension yield x(i) + y(i)).toArray
    def subtract(y: Array[R]) = (for i <- 0 until dimension yield x(i) - y(i)).toArray
    def signum = x.foldLeft(0)((l, r) => if l == 0 then r.signum else l)
    def toCode(level: Level) = s"Array(${x.toList.show(false)})"
    def toMathML = s"<vector>${x.toList.toMathML(false)}</vector>"
  }
  def zero = (for i <- 0 until dimension yield ring.zero).toArray
  override def toString = s"$ring.pow($dimension)"
  def toMathML = s"<msup>${ring.toMathML}<cn>${dimension}</cn></msup>"

  extension (ring: Ring[R]) def pow(n: Int) = {
    assert (n == dimension)
    this
  }
}

object ArrayModule {
  def apply[R : ClassTag](ring: Ring[R])(dimension: Int) = new ArrayModule(using ring)(dimension)
}
