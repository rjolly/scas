package scas.module

import scala.reflect.ClassTag
import scas.Variable
import scas.Implicits.infixRingOps
import ArrayModule.Element

trait ArrayModule[T <: Element[T, R], R] extends scas.structure.Module[T, R] {
  val dimension: Int
  val name: Option[String]
  implicit val m: ClassTag[T]
  implicit val cm: ClassTag[R]
  def generator(n: Int) = apply((for (i <- 0 until dimension) yield if (i == n) ring.one else ring.zero).toArray)
  def generators = (for (i <- 0 until dimension) yield generator(i)).toArray
  override def convert(x: T) = apply((for (i <- 0 until dimension) yield if (i < x.value.length) ring.convert(x(i)) else ring.zero).toArray)
  def apply(l: Long) = apply((for (i <- 0 until dimension) yield ring(l)).toArray)
  override def random(numbits: Int)(implicit rnd: java.util.Random) = apply((for (i <- 0 until dimension) yield ring.random(numbits)).toArray)
  def equiv(x: T, y: T): Boolean = {
    for (i <- 0 until dimension) {
      if (x(i) <> y(i)) return false
    }
    true
  }
  def signum(x: T) = (0 /: x.value) { (l, r) => if (l == 0) ring.signum(r) else l }
  def plus(x: T, y: T) = apply((for (i <- 0 until dimension) yield x(i) + y(i)).toArray)
  def minus(x: T, y: T) = apply((for (i <- 0 until dimension) yield x(i) - y(i)).toArray)
  def ltimes(x: R, y: T) = apply((for (i <- 0 until dimension) yield x * y(i)).toArray)
  override def toCode(x: T, precedence: Int) = name match {
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
  def toMathML(x: T) = name match {
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
  def apply(value: Array[R]): T
  def apply(s: R*): T = apply(s.toArray)
}

object ArrayModule {
  trait Element[T <: Element[T, R], R] extends scas.structure.Module.Element[T, R] with (Int => R) { this: T =>
    val factory: ArrayModule[T, R]
    val value: Array[R]
    def apply(n: Int) = value(n)
  }
}
