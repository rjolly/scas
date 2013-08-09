package scas.module

import scala.reflect.ClassTag
import scas.Implicits.infixRingOps
import AbstractModule.Element

trait AbstractModule[T <: Element[T, R], R] extends scas.structure.Module[T, R] {
  val dimension: Int
  val name: Option[String]
  implicit val m: ClassTag[T]
  implicit val cm: ClassTag[R]
  def generator(n: Int) = apply((for (i <- 0 until dimension) yield if (i == n) ring.one else ring.zero).toArray)
  def convert(x: T) = apply((for (i <- 0 until dimension) yield if (i < x.value.length) ring.convert(x(i)) else ring.zero).toArray)
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
  def times(x: T, y: R) = apply((for (i <- 0 until dimension) yield x(i) * y).toArray)
  def times(x: R, y: T) = apply((for (i <- 0 until dimension) yield x * y(i)).toArray)
  def apply(value: Array[R]): T
  def apply(s: R*): T = apply(s.toArray)
}

object AbstractModule {
  trait Element[T <: Element[T, R], R] extends scas.structure.Module.Element[T, R] { this: T =>
    val factory: AbstractModule[T, R]
    val value: Array[R]
    def apply(n: Int) = value(n)
  }
}
