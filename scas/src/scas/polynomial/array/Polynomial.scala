package scas.polynomial.array

import scas.structure.Ring
import scala.reflect.ClassTag
import scas.power.offset.PowerProduct
import scas.polynomial.ArrayPolynomial
import ArrayPolynomial.Element

class Polynomial[C, N](using val ring: Ring[C], val pp: PowerProduct[N])(using val cm1: ClassTag[N], val cm2: ClassTag[C]) extends ArrayPolynomial[C, N] with Ring.Conv[Element[C, N]] {
  given instance: Polynomial[C, N] = this
}
