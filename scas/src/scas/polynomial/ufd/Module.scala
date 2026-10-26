package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.module.ArrayModule

class Module[T[C, M], C, M](using ring: PolynomialWithGB[T, C, M])(name: String, dimension: Int)(using ClassTag[T[C, M]]) extends ArrayModule[T[C, M]](dimension) {
  def gb(xs: Array[T[C, M]]*) = {
    val s = ring.embedding(name, dimension)
    s.gb((xs.map(_.convertTo(using s)) ++ products(using s))*).map(_.convertFrom(s)).filter(!_.isZero)
  }
  def products(using s: PolynomialWithGB[T, C, M]) = {
    import s.pp
    for i <- 0 until dimension; j <- i until dimension yield s.generator(pp.nbvars - dimension + i) * s.generator(pp.nbvars - dimension + j)
  }
  extension (x: Array[T[C, M]]) def convertTo(using s: PolynomialWithGB[T, C, M]): T[C, M] = {
    x.zipWithIndex.foldLeft(s.zero)((l, r) => {
      import ring.pp
      val (p, n) = r
      l + p.convert(pp) * s.generator(pp.nbvars + n)
    })
  }
  extension (x: T[C, M]) def convertFrom(s: PolynomialWithGB[T, C, M]): Array[T[C, M]] = s.iterator(x).foldLeft(zero) { (l, r) =>
    import s.pp
    val (m, c) = r
    val n = m.projection(pp.nbvars - dimension, pp.nbvars)
    l + (for i <- 0 until dimension yield {
      if n >< pp.generator(pp.nbvars - dimension + i) then s(m / n, c).convert(ring.pp) else ring.zero
    }).toArray
  }
}
