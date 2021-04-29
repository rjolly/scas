package math3.conversion

import scas.structure.conversion.{Algebra, Field}
import math3.Matrix.Element
import Double.given

class Matrix(size: Int) extends math3.Matrix(size) with Algebra[Element, Double] with Field[Element] {
  given Matrix = this

  given int2matrix: (Int => Element) = one%* _
  given double2matrix: (Double => Element) = one%* _
}
