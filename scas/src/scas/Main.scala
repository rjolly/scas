package scas

import scas.polynomial.Polynomial
import scas.util.{Conversion, unary_~}

given identity[T]: (T => T) = x => x

given coef2poly[T, C, M, D : Conversion[C]](using factory: Polynomial[T, C, M]): (D => T) = x => factory(~x)
