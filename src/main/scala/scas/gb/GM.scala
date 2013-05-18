package scas.gb

import scala.collection.SortedSet
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering
import scas.polynomial.{PolynomialOverUFD, PolynomialWithGB}
import scas.Implicits.{infixOrderingOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait GM[T <: Element[T, C, N], C, N] extends Engine[T, C, N] { this: PolynomialWithGB[T, C, N] =>
  type P <: Pair

  class Pair(i: Int, j: Int) extends super.Pair(i, j) { this: P =>
    def |(that: P) = factorOf(this, that)
    override def b_criterion = false
  }

  def factorOf(p1: P, p2: P) = (p1.scm | p2.scm) && (p1.scm < p2.scm)

  override def make(index: Int): Unit = {
    val buffer = new ArrayBuffer[P]
    buffer ++= pairs
    for (k <- 0 until buffer.size) {
      val pair = buffer(k)
      val p1 = apply(pair.i, index)
      val p2 = apply(pair.j, index)
      if ((p1 | pair) && (p2 | pair)) pair.remove
    }
    var s = SortedSet.empty[P](natural)
    for (i <- 0 until index) {
      val pair = apply(i, index)
      s += pair
      pair.add
    }
    buffer.clear
    buffer ++= s
    for (i <- 0 until buffer.size) {
      val p1 = buffer(i)
      for (j <- i + 1 until buffer.size) {
        val p2 = buffer(j)
        if (p1.scm | p2.scm) p2.remove
      }
    }
  }

  def natural = Ordering by { pair: P => pair.key }
}
