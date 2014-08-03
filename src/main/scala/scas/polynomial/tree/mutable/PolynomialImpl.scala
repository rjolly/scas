package scas.polynomial.tree.mutable

import scala.reflect.ClassTag
import scas.power.PowerProduct
import scas.structure.Ring
import Polynomial.Element

class PolynomialImpl[C, N](val ring: Ring[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]]) extends Polynomial[C, N]
