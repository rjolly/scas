package scas.gb

import scala.math.Ordering
import scas.polynomial.PolynomialWithSugar
import PolynomialWithSugar.Element

trait Sugar[T <: Element[T, C, N], C, N] extends GM[T, C, N] {
  override val ring: PolynomialWithSugar[T, C, N]
  import ring.pp

  type P <: Pair

  class Pair(i: Int, j: Int) extends super.Pair(i, j) { this: P =>
    def skey = (sugar, scm, i, j)
    def sugar = 0l
  }

  override def ordering = Ordering by { pair: P => pair.skey }
}

object Sugar {
  def apply[T <: Element[T, C, N], C, N](ring: PolynomialWithSugar[T, C, N]): Sugar[T, C, N] = new Sloppy(ring)
}
