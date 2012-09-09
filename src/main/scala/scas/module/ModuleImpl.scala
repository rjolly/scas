package scas.module

import scala.reflect.ClassTag
import scas.structure.Ring

class ModuleImpl[R](val dimension: Int, val name: Option[String])(implicit val ring: Ring[R], val cm: ClassTag[R]) extends Module[R]
