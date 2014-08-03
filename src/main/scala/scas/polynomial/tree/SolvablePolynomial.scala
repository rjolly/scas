package scas.polynomial.tree

import scala.reflect.ClassTag
import scas.power.PowerProduct
import scas.structure.Ring
import Polynomial.Element

class SolvablePolynomial[C, N](val ring: Ring[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]]) extends Polynomial[C, N] with scas.polynomial.SolvablePolynomial[Element[C, N], C, N]
