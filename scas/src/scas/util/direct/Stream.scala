package scas.util.direct

import scala.concurrent.Future
import scas.util.{await, given}

trait Stream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: Stream[A]

  def force: Stream[A] =
    var these = this
    while !these.isEmpty do these = these.tail
    this

  def filter(p: A => Boolean): Stream[A] = {
    var rest = this
    while !rest.isEmpty && !p(rest.head) do rest = rest.tail
    if !rest.isEmpty then {
      rest.head #: rest.tail.filter(p)
    } else Stream.Nil
  }

  def take(n: Int): Stream[A] = if !isEmpty && n > 0 then {
    head #: tail.take(n - 1)
  } else Stream.Nil

  def takeWhile(p: A => Boolean): Stream[A] = if !isEmpty && p(head) then {
    head #: tail.takeWhile(p)
  } else Stream.Nil

  def exists(p: A => Boolean): Boolean = {
    var these = this
    while !these.isEmpty do {
      if p(these.head) then return true
      these = these.tail
    }
    false
  }

  def size = iterator.size

  def iterator: Iterator[A] = new Stream.Iterator(this)
}

object Stream {
  object Nil extends Stream[Nothing] {
    def isEmpty = true
    def head = ???
    def tail = ???
  }

  final class Cons[+A](hd: A, tl: Future[Stream[A]]) extends Stream[A] {
    def isEmpty = false
    def head = hd
    def tail = tl.await
  }

  extension [A](x: A)
    def #:(xs1: => Stream[A]): Stream[A] =
      Cons(x, Future(xs1))

  def apply[A](s: A*): Stream[A] = if !s.isEmpty then s.head #: apply(s.tail*) else Nil

  def range[T : Integral as num](start: T, end: T, step: T): Stream[T] = {
    import num._
    if if step < zero then start <= end else end <= start then Nil
    else start #: range(start + step, end, step)
  }

  def range[T : Integral as num](start: T, end: T): Stream[T] = range(start, end, num.one)

  trait Sequential[+A] extends Stream[A] {
    override def filter(p: A => Boolean): Stream[A] = {
      var rest: Stream[A] = this
      while !rest.isEmpty && !p(rest.head) do rest = rest.tail
      if !rest.isEmpty then {
        rest.head #:: rest.tail.filter(p)
      } else Stream.Nil
    }

    override def take(n: Int): Stream[A] = if !isEmpty && n > 0 then {
      head #:: tail.take(n - 1)
    } else Stream.Nil

    override def takeWhile(p: A => Boolean): Stream[A] = if !isEmpty && p(head) then {
      head #:: tail.takeWhile(p)
    } else Stream.Nil
  }

  object Sequential {
    final class Cons[+A](hd: A, tl: => Stream[A]) extends Sequential[A] {
      def isEmpty = false
      def head = hd
      lazy val tail = tl
    }

    def apply[A](s: A*): Stream[A] = if !s.isEmpty then s.head #:: apply(s.tail*) else Nil

    def from(start: Int, step: Int): Stream[Int] = start #:: from(start + step, step)

    def from(start: Int): Stream[Int] = from(start, 1)
  }

  extension [A](x: A)
    def #::(xs1: => Stream[A]): Stream[A] =
      Sequential.Cons(x, xs1)

  private class Iterator[+A](private var stream: Stream[A]) extends scala.collection.Iterator[A] {
    override def hasNext: Boolean = !stream.isEmpty

    override def next(): A =
      if stream.isEmpty then Iterator.empty.next()
      else {
        val res = stream.head
        stream = stream.tail
        res
      }
  }
}
