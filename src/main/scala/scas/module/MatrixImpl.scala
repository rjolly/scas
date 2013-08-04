package scas.module

import scala.reflect.ClassTag
import scas.structure.Field
import Matrix.Element

class MatrixImpl[R](val size: Int, val name: Option[String], val ring: Field[R])(implicit val m: ClassTag[Element[R]], val cm: ClassTag[R]) extends Matrix[R]
