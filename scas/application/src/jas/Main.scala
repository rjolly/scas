package jas

import edu.jas.poly.GenPolynomialRing
import edu.jas.structure.RingElem

extension [C <: RingElem[C]](factory: GenPolynomialRing[C]) def toScas = new PolynomialRing(factory)
