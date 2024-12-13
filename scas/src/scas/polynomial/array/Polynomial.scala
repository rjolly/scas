package scas.polynomial.array

import scas.structure.Ring
import scala.reflect.ClassTag
import scas.power.offset.PowerProduct
import scas.polynomial.ArrayPolynomial
import ArrayPolynomial.Element

class Polynomial[C, N](using Ring[C], PowerProduct[N])(using ClassTag[N], ClassTag[C]) extends ArrayPolynomial[C, N] with Ring.Conv[Element[C, N]] {
  given instance: Polynomial[C, N] = this
}
