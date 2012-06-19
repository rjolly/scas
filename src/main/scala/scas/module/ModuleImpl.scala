package scas.module

import scas.structure.Ring

class ModuleImpl[R](val dimension: Int, val name: Option[String])(implicit val ring: Ring[R], val cm: ClassManifest[R]) extends Module[R]
