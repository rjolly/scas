package scas.polynomial.conversion

import scas.util.{Conversion, unary_~}

trait Polynomial[T, C, M] extends scas.polynomial.Polynomial[T, C, M] with scas.structure.Ring[T] {
  given coef2poly[D: Conversion[C]]: (D => T) = x => this(~x)
}
