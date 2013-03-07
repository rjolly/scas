package scas.list

import scala.reflect.ClassTag
import scas.polynomial.PowerProduct
import scas.structure.Ring
import Polynomial.Element

class StreamPolynomial[C, @specialized(Int, Long) N](val ring: Ring[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]]) extends Polynomial[C, N] with scas.polynomial.StreamPolynomial[Element[C, N], C, N]
