package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.module.ArrayModule
import scas.power.POT.Element

class Module[T[C, M], C, M](using ring: PolynomialWithGB[T, C, M])(name: String, dimension: Int)(using ClassTag[T[C, M]]) extends ArrayModule[T[C, M]](dimension) {
  def nbvars = ring.pp.nbvars
  val s = ring.embedding(name, dimension)
  import s.pp
  given PolynomialWithGB[T, C, Element[M]] = s
  def gb(xs: Array[T[C, M]]*) = {
    s.gb((xs.map(_.convertTo) ++ products)*).map(_.convertFrom).filter(!_.isZero)
  }
  def products = {
    for i <- 0 until dimension; j <- i until dimension yield s.generator(nbvars + i) * s.generator(nbvars + j)
  }
  extension (x: Array[T[C, M]]) def convertTo: T[C, Element[M]] = {
    x.zipWithIndex.foldLeft(s.zero) { (l, r) =>
      val (p, n) = r
      l + p.convert * s.generator(nbvars + n)
    }
  }
  extension (x: T[C, M]) def convert = x.iterator.foldLeft(s.zero) { (l, r) =>
    val (m, c) = r
    l + s((m, pp.one._2), c)
  }
  extension (x: T[C, Element[M]]) def convertFrom: Array[T[C, M]] = x.iterator.foldLeft(zero) { (l, r) =>
    val (m, c) = r
    val n = m.projection(nbvars, nbvars + dimension)
    l + (for i <- 0 until dimension yield {
      if n >< pp.generator(nbvars + i) then ring(m._1, c) else ring.zero
    }).toArray
  }
}
