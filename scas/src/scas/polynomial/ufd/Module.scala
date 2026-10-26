package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.module.ArrayModule
import scas.power.POT.Element

class Module[T[C, M], C, M](using ring: PolynomialWithGB[T, C, M])(name: String, dimension: Int)(using ClassTag[T[C, M]]) extends ArrayModule[T[C, M]](dimension) {
  def gb(xs: Array[T[C, M]]*) = {
    val s = ring.embedding(name, dimension)
    s.gb((xs.map(_.convertTo(using s)) ++ products(using s))*).map(_.convertFrom(s)).filter(!_.isZero)
  }
  def products(using s: PolynomialWithGB[T, C, Element[M]]) = {
    import ring.pp
    for i <- 0 until dimension; j <- i until dimension yield s.generator(pp.nbvars + i) * s.generator(pp.nbvars + j)
  }
  extension (x: Array[T[C, M]]) def convertTo(using s: PolynomialWithGB[T, C, Element[M]]): T[C, Element[M]] = {
    x.zipWithIndex.foldLeft(s.zero) { (l, r) =>
      import ring.pp
      val (p, n) = r
      l + p.convert * s.generator(pp.nbvars + n)
    }
  }
  extension (x: T[C, M]) def convert(using s: PolynomialWithGB[T, C, Element[M]]) = x.iterator.foldLeft(s.zero) { (l, r) =>
    import s.pp
    val (m, c) = r
    l + s((m, pp.one._2), c)
  }
  extension (x: T[C, Element[M]]) def convertFrom(s: PolynomialWithGB[T, C, Element[M]]): Array[T[C, M]] = s.iterator(x).foldLeft(zero) { (l, r) =>
    import s.pp
    val (m, c) = r
    val n = m.projection(ring.pp.nbvars, ring.pp.nbvars + dimension)
    l + (for i <- 0 until dimension yield {
      if n >< pp.generator(ring.pp.nbvars + i) then ring(m._1, c) else ring.zero
    }).toArray
  }
}
