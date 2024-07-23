package scas.polynomial

import scala.reflect.ClassTag
import scas.module.ArrayModule
import scas.structure.Ring
import scas.power.ModifiedPOT
import scas.math.Numeric
import scas.variable.Variable

class Module[T : ClassTag, C, N : Numeric : ClassTag](using val ring: PolynomialWithGB[T, C, Array[N]])(name: String, dimension: Int) extends ArrayModule[T](dimension) {
  def gb(xs: Array[T]*) = {
    val s = ring.newInstance(new ModifiedPOT(ring.pp, name, dimension))
    s.gb((xs.map(_.convertTo(using s)) ++ products(using s))*).map(_.convertFrom(s))
  }
  def products(using s: PolynomialWithGB[T, C, Array[N]]) = for (i <- 0 until dimension; j <- i until dimension) yield s.generator(s.length + i) * s.generator(s.length + j)
  extension (x: Array[T]) def convertTo(using s: PolynomialWithGB[T, C, Array[N]]): T = {
    x.zipWithIndex.foldLeft(s.zero)((l, r) => {
      val (p, n) = r
      l + p.convert(ring.pp) * s.generator(ring.length + n)
    })
  }
  extension (x: T) def convertFrom(s: PolynomialWithGB[T, C, Array[N]]): Array[T] = s.iterator(x).foldLeft(zero) { (l, r) =>
    val (m, c) = r
    l + (for (i <- 0 until dimension) yield {
      import s.pp
      val n = m.projection(pp.length + i)
      if (n.isOne) ring.zero else s(m / n, c).convert(pp)
    }).toArray
  }
}
