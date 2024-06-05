package math3.conversion

import scas.structure.conversion.{AlgebraOverRing, Field}
import math3.Matrix.Element
import math3.Double
import Double.given

class Matrix(val size: Int) extends math3.Matrix with AlgebraOverRing[Element, Double] with Field[Element] {
  given instance: Matrix = this
}
