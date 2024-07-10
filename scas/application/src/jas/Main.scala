package jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.poly.GenPolynomial
import edu.jas.structure.RingElem
import scas.util.{Conversion, unary_~}

given [C <: RingElem[C] : GenPolynomialRing] => conversion.PolynomialRing[C] as poly2scas

given [C <: RingElem[C] : GenPolynomialRing, D : Conversion[C]] => (D => GenPolynomial[C]) as coef2poly = x => summon[GenPolynomialRing[C]].valueOf(~x)
