package scas.adapter.jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scas.util.{Conversion, unary_~}

given poly2scas: [C <: RingElem[C] : GenPolynomialRing] => PolynomialRing.Conv[C] = new PolynomialRing.Conv

given coef2poly: [C <: RingElem[C] : GenPolynomialRing, D : Conversion[C]] => (D => GenPolynomial[C]) = x => summon[GenPolynomialRing[C]].valueOf(~x)
