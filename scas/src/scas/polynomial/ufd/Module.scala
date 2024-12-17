package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.module.ArrayModule
import scas.power.ModifiedPOT
import scas.math.Numeric

class Module[T : ClassTag, C, N : Numeric : ClassTag](using ring: PolynomialWithGB[T, C, N])(name: String, dimension: Int) extends ArrayModule[T](dimension) {
  def gb(xs: Array[T]*) = {
    val s = ring.newInstance(new ModifiedPOT(ring.pp, name, dimension))
    s.gb((xs.map(_.convertTo(using s)) ++ products(using s))*).map(_.convertFrom(s)).filter(!_.isZero)
  }
  def products(using s: PolynomialWithGB[T, C, N]) = {
    import s.pp
    for (i <- 0 until dimension; j <- i until dimension) yield s.generator(pp.length + i) * s.generator(pp.length + j)
  }
  extension (x: Array[T]) def convertTo(using s: PolynomialWithGB[T, C, N]): T = {
    x.zipWithIndex.foldLeft(s.zero)((l, r) => {
      import ring.pp
      val (p, n) = r
      l + p.convert(pp) * s.generator(pp.length + n)
    })
  }
  extension (x: T) def convertFrom(s: PolynomialWithGB[T, C, N]): Array[T] = s.iterator(x).foldLeft(zero) { (l, r) =>
    import s.pp
    val (m, c) = r
    val n = m.projection(pp.length, pp.length + dimension)
    l + (for (i <- 0 until dimension) yield {
      if (n >< pp.generator(pp.length + i)) s(m / n, c).convert(pp) else ring.zero
    }).toArray
  }
}
