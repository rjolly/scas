package scas.polynomial

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering

trait GMSetting[T, C, N, P[N] <: Pair[N]](using factory: PolynomialWithGB[T, C, N]) extends Engine[T, C, N, P] {
  import factory.pp

  override def b_criterion(pa: P[N]) = false

  extension (p1: P[N]) def | (p2: P[N]) = (p1.scm | p2.scm) && (p1.scm < p2.scm)

  override def make(index: Int): Unit = {
    val buffer = new ArrayBuffer[P[N]]
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

  def natural = Ordering by { (pair: P[N]) => pair.key }
}
