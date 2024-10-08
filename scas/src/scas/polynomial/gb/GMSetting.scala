package scas.polynomial.gb

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ArrayBuffer
import scas.polynomial.Polynomial
import scala.math.Ordering

trait GMSetting[T, C, M, P[M] <: Pair[M]](using factory: Polynomial[T, C, M]) extends Engine[T, C, M, P] {
  import factory.pp

  override def b_criterion(pa: P[M]) = false

  extension (p1: P[M]) def | (p2: P[M]) = (p1.scm | p2.scm) && (p1.scm < p2.scm)

  override def make(index: Int): Unit = {
    val buffer = new ArrayBuffer[P[M]]
    for (pair <- pairs) {
      val p1 = apply(pair.i, index)
      val p2 = apply(pair.j, index)
      if ((p1 | pair) && (p2 | pair)) buffer += pair
    }
    for (i <- 0 until buffer.size) remove(buffer(i))
    var s = SortedSet.empty(using natural)
    for (i <- 0 until index) {
      val pair = apply(i, index)
      s += pair
      add(pair)
    }
    buffer.clear
    buffer ++= s
    for (i <- 0 until buffer.size) {
      val p1 = buffer(i)
      for (j <- i + 1 until buffer.size) {
        val p2 = buffer(j)
        if (p1.scm | p2.scm) remove(p2)
      }
    }
  }

  def natural = Ordering by { (pair: P[M]) => pair.key }
}
