package scas.structure.ordered

trait Monoid[T] extends Monoid.Impl[T] with scas.structure.Monoid[T] with Structure[T]

object Monoid {
  trait Impl[T] extends scas.structure.Monoid.Impl[T] with Structure.Impl[T]
}
