package scas.polynomial.array

import scala.reflect.ClassTag
import scas.gb.Sloppy
import scas.power.offset.PowerProduct
import scas.structure.UniqueFactorizationDomain
import PolynomialWithSugar.Element

class PolynomialWithSloppy[C, N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassTag[Element[C, N]], val cm1: ClassTag[C], val cm2: ClassTag[N]) extends PolynomialWithSugar[C, N] with Sloppy[Element[C, N], C, N]
