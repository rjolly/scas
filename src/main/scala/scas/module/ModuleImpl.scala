package scas.module

import scas.Variable
import scas.structure.Ring

class ModuleImpl[R](val variables: Array[Variable])(implicit val ring: Ring[R], val cm: ClassManifest[R]) extends Module[R]
