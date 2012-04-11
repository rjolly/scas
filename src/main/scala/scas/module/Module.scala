package scas.module

import scas.Variable
import scas.structure.Ring
import scas.Implicits.infixRingOps
import Module.Element

class Module[R](val variables: Array[Variable])(implicit val ring: Ring[R], val cm: ClassManifest[R]) extends scas.structure.Module[Element[R], R] {
  def generator(n: Int) = apply((for (i <- 0 until length) yield if (i == n) ring.one else ring.zero).toArray)
  def generators = (for (i <- 0 until length) yield generator(i)).toArray
  def apply(x: Element[R]) = apply(x.value)
  def apply(l: Long) = apply((for (i <- 0 until length) yield ring(l)).toArray)
  override def random(numbits: Int)(implicit rnd: java.util.Random) = apply((for (i <- 0 until length) yield ring.random(numbits)).toArray)
  def compare(x: Element[R], y: Element[R]): Int = {
    for (i <- 0 until length) {
      val s = ring.compare(x.value(i), y.value(i))
      if (s < 0) return -1
      else if (s > 0) return 1
    }
    0
  }
  def plus(x: Element[R], y: Element[R]) = apply((for (i <- 0 until length) yield x.value(i) + y.value(i)).toArray)
  def minus(x: Element[R], y: Element[R]) = apply((for (i <- 0 until length) yield x.value(i) - y.value(i)).toArray)
  def rtimes(x: Element[R], y: R) = apply((for (i <- 0 until length) yield x.value(i) * y).toArray)
  def ltimes(x: R, y: Element[R]) = apply((for (i <- 0 until length) yield x * y.value(i)).toArray)
  override def toCode(x: Element[R], precedence: Int) = {
    var s = ""
    var n = 0
    var m = 0
    for (i <- 0 until length) {
      val c = ring.abs(x.value(i))
      val (t, u) = {
        if (c.isZero) ("", 0)
        else if (c.isOne) (variables(i), 1)
        else (c.toCode(1) + "*" + variables(i), 2)
      }
      if (u > 0) {
        s = s + (if (ring.signum(x.value(i)) < 0) "-" else (if (n == 0) "" else "+")) + t
        m = u + (if (ring.signum(x.value(i)) < 0) 1 else 0)
        n += 1
      }
    }
    if (n == 0) ring.zero.toCode(0) else {
      val fenced = {
        if (n == 1) {
          if (m == 1) false
          else precedence > 1
        } else precedence > 0
      }
      if (fenced) "(" + s + ")" else s
    }
  }
  override def toString = ring.toString + "^" + length
  def apply(value: Array[R]) = new Element((for (i <- 0 until length) yield if (i < value.length) ring(value(i)) else ring.zero).toArray)(this)

  def length = variables.length
}

object Module {
  def apply[R](name: String, dimension: Int, ring: Ring[R])(implicit cm: ClassManifest[R]) = new Module((for (i <- 0 until dimension) yield Variable(name, i)).toArray)(ring, cm)

  class Element[R](val value: Array[R])(val factory: Module[R]) extends scas.structure.Module.Element[Element[R], R]
  implicit def ring2scalar[S <% R, R: Module](value: S) = implicitly[Module[R]].scalar(value)
}
