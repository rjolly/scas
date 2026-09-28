package scas.polynomial.gb

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ArrayBuffer
import scas.polynomial.Polynomial
import scas.power.PowerProduct
import scas.math.Ordering
import GMEngine.Impl
import GBEngine.Pair

class GMEngine[T, C, M : PowerProduct](using factory: Polynomial[T, C, M]) extends GBEngine[T, C, M] with Impl[T, C, M, Pair[M]] {
  def this(factory: Polynomial[T, C, M]) = this(using factory.pp, factory)
}

object GMEngine {
  trait Impl[T, C, M : PowerProduct, P <: Pair[M]](using factory: Polynomial[T, C, M]) extends GBEngine.Impl[T, C, M, P] {
    override def b_criterion(pa: P) = false

    extension (p1: P) def | (p2: P) = (p1.scm | p2.scm) && (p1.scm < p2.scm)

    override def make(index: Int): Unit = {
      val buffer = new ArrayBuffer[P]
      for pair <- pairs do {
        val p1 = apply(pair.i, index)
        val p2 = apply(pair.j, index)
        if (p1 | pair) && (p2 | pair) then buffer += pair
      }
      for i <- 0 until buffer.size do remove(buffer(i))
      var s = SortedSet.empty(using natural)
      for i <- 0 until index do {
        val pair = apply(i, index)
        s += pair
        add(pair)
      }
      buffer.clear()
      buffer ++= s
      for i <- 0 until buffer.size do {
        val p1 = buffer(i)
        for j <- i + 1 until buffer.size do {
          val p2 = buffer(j)
          if p1.scm | p2.scm then remove(p2)
        }
      }
    }

    override def apply(i: Int, j: Int) = super.apply(i, j)

    def natural: Ordering[P] = Ordering by { pair => (pair.scm, pair.j, pair.i) }
  }
}
