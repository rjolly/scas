package scas.module

import scala.reflect.ClassTag
import scas.structure.Ring
import Module.Element

class ModuleImpl[R](val dimension: Int, val name: Option[String], val ring: Ring[R])(implicit val m: ClassTag[Element[R]], val cm: ClassTag[R]) extends Module[R]
