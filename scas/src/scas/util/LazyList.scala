package scas.util

import LazyList.{Nil, #::, Iterator}

trait LazyList[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: LazyList[A]

  def force: LazyList[A] =
    var these = this
    while (!these.isEmpty) these = these.tail
    this

  def iterator: scala.collection.Iterator[A] = new Iterator(this)
}

object LazyList {
  object Nil extends LazyList[Nothing] {
    def isEmpty = true
    def head = ???
    def tail = ???
  }

  final class Cons[+A](hd: A, tl: => LazyList[A]) extends LazyList[A] {
    def isEmpty = false
    def head = hd
    lazy val tail = tl
  }

  extension [A](x: A)
    def #::(xs1: => LazyList[A]): LazyList[A] =
      Cons(x, xs1)

  def apply[A](s: A*): LazyList[A] = if (!s.isEmpty) s.head #:: apply(s.tail*) else LazyList.Nil

  private class Iterator[+A](private var stream: LazyList[A]) extends scala.collection.Iterator[A] {
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
