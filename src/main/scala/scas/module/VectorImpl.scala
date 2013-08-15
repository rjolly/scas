package scas.module

import scala.reflect.ClassTag
import scas.structure.Field
import Vector.Element

class VectorImpl[R](val dimension: Int, val name: Option[String], val ring: Field[R])(implicit val m: ClassTag[Element[R]], val cm: ClassTag[R]) extends Vector[R]
