package scas.module

import scala.reflect.ClassTag
import scas.structure.{Field, VectorSpace}
import Module.Element

class Vector[R](val dimension: Int, val name: Option[String], val ring: Field[R])(implicit val cm: ClassTag[R]) extends Module[R] with VectorSpace[Element[R], R]
