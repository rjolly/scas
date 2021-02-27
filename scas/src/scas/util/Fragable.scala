package scas.util

import scala.reflect.ClassTag

trait Fragable[S, T : ClassTag]:
  extension (x: S)
    def toFrags: Array[T]

object Fragable:
  given [U, T : ClassTag](using c: U => T): Fragable[U, T] with
    extension (x: U)
      def toFrags = Array(c(x))
  given [T : ClassTag]: Fragable[EmptyTuple, T] with
    extension (x: EmptyTuple)
      def toFrags = Array()
  given [A, B <: Tuple, T : ClassTag](using Fragable[A, T], Fragable[B, T]): Fragable[A *: B, T] with
    extension (x: A *: B)
      def toFrags = x.head.toFrags ++ x.tail.toFrags
