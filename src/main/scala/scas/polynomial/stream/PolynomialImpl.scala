package scas.polynomial
package stream

import scala.reflect.ClassTag
import scas.structure.Ring
import Polynomial.Element

class PolynomialImpl[C, @specialized(Int, Long) N](val ring: Ring[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]]) extends Polynomial[C, N]
