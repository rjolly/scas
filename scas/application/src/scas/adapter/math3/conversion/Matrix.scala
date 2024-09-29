package scas.adapter.math3.conversion

import scas.structure.conversion.{AlgebraOverRing, Field}
import scas.adapter.math3.Matrix.Element
import scas.adapter.math3.Double
import Double.given

class Matrix(val size: Int) extends scas.adapter.math3.Matrix with AlgebraOverRing[Element, Double] with Field[Element] {
  given instance: Matrix = this
}
