package math3.conversion

import scas.structure.{Algebra, Field}
import math3.Matrix.Element
import math3.Double
import Double.given

class Matrix(val size: Int) extends math3.Matrix with Algebra[Element, Double] with Field[Element] {
  given instance: Matrix = this

  given int2matrix: (Int => Element) = one%* _
  given double2matrix: (Double => Element) = one%* _
}
