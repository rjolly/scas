package scas.structure

import scas.structure.Ring

trait Module[T, R: Ring] extends AbelianGroup[T] with
  def (x: R):*:(y: T): T
  def (x: T):* (y: R): T
