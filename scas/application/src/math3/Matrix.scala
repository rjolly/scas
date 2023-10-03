package math3

import org.apache.commons.math3.linear.RealMatrix
import scas.structure.{Algebra, Field}
import scas.base.BigInteger.given
import Matrix.Element
import Double.given

class Matrix(val size: Int) extends impl.Matrix with Algebra[Element, Double] with Field[Element] {
  given instance: Matrix = this

  given int2matrix: (Int => Element) = one%* _
  given double2matrix: (Double => Element) = one%* _
}

object Matrix {
  type Element = RealMatrix
}
