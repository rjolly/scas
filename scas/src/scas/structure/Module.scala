package scas.structure

import scas.structure.Ring

abstract class Module[T, R: Ring] extends AbelianGroup[T] with
  def (x: R):*:(y: T): T
  def (x: T):* (y: R): T
