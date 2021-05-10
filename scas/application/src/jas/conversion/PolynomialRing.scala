package jas.conversion

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scas.util.Conversion

class PolynomialRing[C <: RingElem[C] : GenPolynomialRing] extends jas.PolynomialRing[C] with Ring[GenPolynomial[C]] {
  given PolynomialRing[C] = this

  given coef2poly[D: Conversion[C]]: (D => GenPolynomial[C]) = jas.conversion.coef2poly
}
