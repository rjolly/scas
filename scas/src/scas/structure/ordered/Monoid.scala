package scas.structure.ordered

trait Monoid[T] extends scas.structure.Monoid[T] with Structure[T]

object Monoid {
  trait Ops[T: Monoid] extends scas.structure.SemiGroup.Ops[T] with Structure.Ops[T]
}
