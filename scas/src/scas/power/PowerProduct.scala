package scas.power

import scala.reflect.ClassTag
import scas.structure.ordered.Monoid
import scas.util.{Conversion, unary_~}
import scas.variable.Variable
import PowerProduct.Ops

trait PowerProduct[M: ClassTag] extends Monoid[M] {
  given PowerProduct[M] = this
  given Ops[M] with {}
  def variables: Seq[Variable]
  val length = variables.length
  def generator(variable: String): M = generator(variables.indexOf(variable))
  def generator(n: Int): M
  def generators = (for (i <- 0 until length) yield generator(i)).toArray
  def degree(x: M): Long
  def apply(x: Int) = {
    assert (x == 1)
    one
  }
  def gcd(x: M, y: M): M
  def lcm(x: M, y: M): M
  def coprime(x: M, y: M) = gcd(x, y).isOne
  extension (x: M) {
    def divide(y: M): M
    def factorOf(y: M): Boolean
    inline def / [U: Conversion[M]](y: U) = x.divide(~y)
    inline def | [U: Conversion[M]](y: U) = x.factorOf(~y)
    def isUnit = x.isOne
  }
  def dependencyOnVariables(x: M): Array[Int]
  extension (x: M) def projection(n: Int): M
  override def toString = s"List(${variables.map(_.toString).mkString(", ")})"
  def toMathML = s"<list>${variables.map(_.toMathML).mkString}</list>"
  override def convert(x: M) = x.convert(variables)
  extension (x: M) def convert(from: Seq[Variable]): M
  def size(x: M): Int

  given int2powerProduct: (Int => M) = apply(_)
}

object PowerProduct {
  trait Ops[M: PowerProduct] extends Monoid.Ops[M] {
    extension[U: Conversion[M]] (x: U) {
      inline def / (y: M) = (~x).divide(y)
      inline def | (y: M) = (~x).factorOf(y)
    }
  }
}
