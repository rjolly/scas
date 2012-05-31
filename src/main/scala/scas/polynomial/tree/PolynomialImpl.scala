package scas.polynomial.tree

import scas.polynomial.PowerProduct
import scas.structure.Ring
import Polynomial.Element

class PolynomialImpl[C, @specialized(Int, Long) N](val ring: Ring[C], val pp: PowerProduct[N])(implicit val cm: ClassManifest[Element[C, N]]) extends Polynomial[C, N]
