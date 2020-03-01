package scas.structure

import scas.math.Equiv
import scas.prettyprint.Show

trait Structure[T] extends Equiv[T] with Show[T]
  def random(numbits: Int)(using rnd: java.util.Random): T = ???

object Structure with
  def apply[T: Structure] = summon[Structure[T]]

  trait Ops[T] with
    def x: T
    def factory: Structure[T]
    given Structure[T] = factory

  class OpsImpl[T: Structure](val x: T) extends Ops[T] with
    def factory = Structure[T]
