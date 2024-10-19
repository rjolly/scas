package scas.util

import scala.concurrent.Future

trait Stream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: Stream[A]

  def force: Stream[A] =
    var these = this
    while (!these.isEmpty) these = these.tail
    this

  def iterator: Iterator[A] = new Stream.Iterator(this)
}

object Stream {
  object Nil extends Stream[Nothing] {
    def isEmpty = true
    def head = ???
    def tail = ???
  }

  final class Cons[+A](hd: A, tl: Future[Stream[A]]) extends Stream[A] {
    def this(hd: A, tail: => Stream[A]) = this(hd, Future(tail))
    def isEmpty = false
    def head = hd
    def tail = tl.await
  }

  extension [A](x: A)
    def #:(xs1: => Stream[A]): Stream[A] =
      Cons(x, xs1)

  def apply[A](s: A*): Stream[A] = if (!s.isEmpty) s.head #: apply(s.tail*) else Stream.Nil

  private class Iterator[+A](private var stream: Stream[A]) extends scala.collection.Iterator[A] {
    override def hasNext: Boolean = !stream.isEmpty

    override def next(): A =
      if (stream.isEmpty) Iterator.empty.next()
      else {
        val res = stream.head
        stream = stream.tail
        res
      }
  }
}
