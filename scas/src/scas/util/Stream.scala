package scas.util

import scala.concurrent.Future

trait Stream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: Future[Stream[A]]

  def force: Stream[A] =
    var these = this
    while !these.isEmpty do these = these.tail.await
    this

  def filter(p: A => Boolean): Stream[A] = {
    var rest = this
    while !rest.isEmpty && !p(rest.head) do rest = rest.tail.await
    if !rest.isEmpty then {
      rest.head #: rest.tail.map(_.filter(p))
    } else Stream.Nil
  }

  def take(n: Int): Stream[A] = if !isEmpty && n > 0 then {
    head #: tail.map(_.take(n - 1))
  } else Stream.Nil

  def takeWhile(p: A => Boolean): Stream[A] = if !isEmpty && p(head) then {
    head #: tail.map(_.takeWhile(p))
  } else Stream.Nil

  def exists(p: A => Boolean): Boolean = {
    var these = this
    while !these.isEmpty do {
      if p(these.head) then return true
      these = these.tail.await
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
    def tail = tl
  }

  extension [A](x: A)
    def #:(xs1: Future[Stream[A]]): Stream[A] =
      Cons(x, xs1)

  def apply[A](s: A*): Stream[A] = if !s.isEmpty then s.head #: Future(apply(s.tail*)) else Nil

  def range[T : Integral as num](start: T, end: T, step: T): Stream[T] = {
    import num._
    if if step < zero then start <= end else end <= start then Nil
    else start #: Future(range(start + step, end, step))
  }

  def range[T : Integral as num](start: T, end: T): Stream[T] = range(start, end, num.one)

  object sequential {
    def apply[A](s: A*): Stream[A] = if !s.isEmpty then s.head #: Lazy(apply(s.tail*)) else Nil

    def from(start: Int, step: Int): Stream[Int] = start #: Lazy(from(start + step, step))

    def from(start: Int): Stream[Int] = from(start, 1)
  }

  private class Iterator[+A](private var stream: Stream[A]) extends scala.collection.Iterator[A] {
    override def hasNext: Boolean = !stream.isEmpty

    override def next(): A =
      if stream.isEmpty then Iterator.empty.next()
      else {
        val res = stream.head
        stream = stream.tail.await
        res
      }
  }
}
