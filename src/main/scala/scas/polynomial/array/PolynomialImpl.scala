package scas.polynomial
package array

import scala.reflect.ClassTag
import scas.power.PowerProduct
import scas.structure.Ring
import Polynomial.Element

class PolynomialImpl[C, @specialized(Int, Long) N](val ring: Ring[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]], val cm1: ClassTag[C], val cm2: ClassTag[N]) extends Polynomial[C, N]
