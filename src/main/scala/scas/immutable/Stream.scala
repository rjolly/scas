package scas.immutable

import scala.concurrent._
import duration._
import scala.collection._
import generic._
import mutable.{Builder, StringBuilder, LazyBuilder, ListBuffer}
import scala.annotation.tailrec
import Stream.{Cons => cons, Empty}
import ExecutionContext.Implicits.global
import scala.language.implicitConversions

abstract class Stream[+A] extends Object
                             with LinearSeq[A]
                             with GenericTraversableTemplate[A, Stream]
                             with LinearSeqOptimized[A, Stream[A]] {
self =>
  override def companion: GenericCompanion[Stream] = Stream

  import scala.collection.{Traversable, Iterable, Seq, IndexedSeq}

  def isEmpty: Boolean

  def head: A

  def tail: Stream[A]

  protected def tailDefined: Boolean

  // Implementation of abstract method in Traversable

  // New methods in Stream

  def append[B >: A](rest: Future[TraversableOnce[B]]): Stream[B] = this match {
    case cons(head, tail) => cons(head, tail.map(_ append rest))
    case Empty => Stream(Await.result(rest, Duration.Inf).toStream)
  }

  def force: Stream[A] = {
    var these = this
    while (!these.isEmpty) these = these.tail
    this
  }

  override def length: Int = {
    var len = 0
    var left = this
    while (!left.isEmpty) {
      len += 1
      left = left.tail
    }
    len
  }

  @inline private def asThat[That](x: AnyRef): That     = x.asInstanceOf[That]
  @inline private def asStream[B](x: AnyRef): Stream[B] = x.asInstanceOf[Stream[B]]
  @inline private def isStreamBuilder[B, That](bf: CanBuildFrom[Stream[A], B, That]) = bf(repr).isInstanceOf[Stream.StreamBuilder[_]]

  // Overridden methods from Traversable

  override def toStream: scala.Stream[A] = { val Stream(s) = this ; s }

  override def hasDefiniteSize = {
    def loop(s: Stream[A]): Boolean = s.isEmpty || s.tailDefined && loop(s.tail)
    loop(this)
  }

  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    // we assume there is no other builder factory on streams and therefore know that That = Stream[A]
    if (isStreamBuilder(bf)) asThat(this match {
      case cons(head, tail) => cons(head, tail.map(s => asStream[A](s ++ that)))
      case Empty => Stream(that.toStream)
    })
    else super.++(that)(bf)

  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    if (isStreamBuilder(bf)) asThat(cons(elem, future(this)))
    else super.+:(elem)(bf)

  override final def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    if (isStreamBuilder(bf)) asThat(this match {
      case cons(head, tail) => cons(z, tail.map(s => asStream[B](s.scanLeft(op(z, head))(op))))
      case Empty => Stream(z)
    })
    else super.scanLeft(z)(op)(bf)

  override final def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
    if (isStreamBuilder(bf)) asThat(this match {
      case cons(head, tail) => cons(f(head), tail.map(s => asStream[B](s map f)))
      case Empty => Empty
    })
    else super.map(f)(bf)
  }

  override final def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
    if (!isStreamBuilder(bf)) super.collect(pf)(bf)
    else {
      var rest: Stream[A] = this
      while (rest.nonEmpty && !pf.isDefinedAt(rest.head)) rest = rest.tail
      asThat(rest match {
        case cons(head, tail) => cons(pf(head), tail.map(s => asStream[B](s collect pf)))
        case Empty => Empty
      })
    }
  }

  override final def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    if (isStreamBuilder(bf)) asThat(
      if (isEmpty) Stream.Empty
      else {
        var nonEmptyPrefix = this
        var prefix = Stream(f(nonEmptyPrefix.head).toStream)
        while (!nonEmptyPrefix.isEmpty && prefix.isEmpty) {
          nonEmptyPrefix = nonEmptyPrefix.tail
          if(!nonEmptyPrefix.isEmpty)
            prefix = Stream(f(nonEmptyPrefix.head).toStream)
        }
        nonEmptyPrefix match {
          case cons(head, tail) => prefix append tail.map(s => asStream[B](s flatMap f))
          case Empty => Empty
        }
      }
    )
    else super.flatMap(f)(bf)

  override def filter(p: A => Boolean): Stream[A] = {
    var rest = this
    while (!rest.isEmpty && !p(rest.head)) rest = rest.tail
    rest match {
      case cons(head, tail) => cons(head, tail.map(_ filter p))
      case Empty => Empty
    }
  }

  override final def withFilter(p: A => Boolean): StreamWithFilter = new StreamWithFilter(p)

  final class StreamWithFilter(p: A => Boolean) extends WithFilter(p) {

    override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
      def tailMap(coll: Stream[A]): Stream[B] = {
        var head: A = null.asInstanceOf[A]
        var tail: Stream[A] = coll
        while (true) {
          tail match {
            case cons(head, tail) if (p(head)) => return cons(f(head), tail.map(tailMap(_)))
            case Empty => return Empty
          }
          head = tail.head
          tail = tail.tail
        }
        throw new RuntimeException()
      }

      if (isStreamBuilder(bf)) asThat(tailMap(Stream.this))
      else super.map(f)(bf)
    }

    override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
      def tailFlatMap(coll: Stream[A]): Stream[B] = {
        var head: A = null.asInstanceOf[A]
        var tail: Stream[A] = coll
        while (true) {
          tail match {
            case cons(head, tail) if (p(head)) => return Stream(f(head).toStream) append tail.map(tailFlatMap(_))
            case Empty => return Empty
          }
          head = tail.head
          tail = tail.tail
        }
        throw new RuntimeException()
      }

      if (isStreamBuilder(bf)) asThat(tailFlatMap(Stream.this))
      else super.flatMap(f)(bf)
    }

    override def foreach[B](f: A => B) =
      for (x <- self)
        if (p(x)) f(x)

    override def withFilter(q: A => Boolean): StreamWithFilter =
      new StreamWithFilter(x => p(x) && q(x))
  }

  override def iterator: Iterator[A] = new StreamIterator(self)

  @tailrec override final def foreach[B](f: A => B) {
    if (!this.isEmpty) {
      f(head)
      tail.foreach(f)
    }
  }

  @tailrec override final def foldLeft[B](z: B)(op: (B, A) => B): B = {
    if (this.isEmpty) z
    else tail.foldLeft(op(z, head))(op)
  }

  override final def reduceLeft[B >: A](f: (B, A) => B): B = {
    if (this.isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    else {
      var reducedRes: B = this.head
      var left = this.tail
      while (!left.isEmpty) {
        reducedRes = f(reducedRes, left.head)
        left = left.tail
      }
      reducedRes
    }
  }

  override def partition(p: A => Boolean): (Stream[A], Stream[A]) = (filter(p(_)), filterNot(p(_)))

  override final def zip[A1 >: A, B, That](that: scala.collection.GenIterable[B])(implicit bf: CanBuildFrom[Stream[A], (A1, B), That]): That =
    // we assume there is no other builder factory on streams and therefore know that That = Stream[(A1, B)]
    if (isStreamBuilder(bf)) asThat(this match {
      case cons(hdi, tli) => that match {
        case cons(hda, tla) => cons((hdi, hda), for (si <- tli; sa <- tla) yield asStream[(A1, B)](si zip sa))
        case Empty => Empty
      }
      case Empty => Empty
    })
    else super.zip(that)(bf)

  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Stream[A], (A1, Int), That]): That =
    this.zip[A1, Int, That](Stream.from(0))

  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    def loop(pre: String, these: Stream[A]) {
      if (these.isEmpty) b append end
      else {
        b append pre append these.head
        if (these.tailDefined) loop(sep, these.tail)
        else b append sep append "?" append end
      }
    }
    b append start
    loop("", this)
    b
  }

  override def mkString(sep: String): String = mkString("", sep, "")
  override def mkString: String = mkString("")
  override def mkString(start: String, sep: String, end: String): String = {
    this.force
    super.mkString(start, sep, end)
  }
  override def toString = super.mkString(stringPrefix + "(", ", ", ")")

  override def splitAt(n: Int): (Stream[A], Stream[A]) = (take(n), drop(n))

  override def take(n: Int): Stream[A] = this match {
    case cons(head, tail) => if (n <= 0) Stream.empty
      else if (n == 1) cons(head, future(Stream.empty))
      else cons(head, tail.map(_ take n-1))
    case Empty => Empty
  }

  @tailrec final override def drop(n: Int): Stream[A] =
    if (n <= 0 || isEmpty) this
    else tail drop n-1

  override def slice(from: Int, until: Int): Stream[A] = {
    val lo = from max 0
    if (until <= lo || isEmpty) Stream.empty
    else this drop lo take (until - lo)
  }

  override def init: Stream[A] =
    if (isEmpty) super.init
    else if (tail.isEmpty) Stream.Empty
    else this match {
      case cons(head, tail) => cons(head, tail.map(_.init))
    }

  override def takeRight(n: Int): Stream[A] = {
    var these: Stream[A] = this
    var lead = this drop n
    while (!lead.isEmpty) {
      these = these.tail
      lead = lead.tail
    }
    these
  }

  // there's nothing we can do about dropRight, so we just keep the definition
  // in LinearSeq

  override def takeWhile(p: A => Boolean): Stream[A] = this match {
    case cons(head, tail) if (p(head)) => cons(head, tail.map(_ takeWhile p))
    case _ => Empty
  }

  override def dropWhile(p: A => Boolean): Stream[A] = {
    var these: Stream[A] = this
    while (!these.isEmpty && p(these.head)) these = these.tail
    these
  }

  override def distinct: Stream[A] = this match {
    case cons(head, tail) => cons(head, tail.map(_.filter(head != _).distinct))
    case Empty => Empty
  }

  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
    def loop(len: Int, these: Stream[A]): Stream[B] = these match {
      case cons(head, tail) => cons(head, tail.map(loop(len - 1, _)))
      case Empty => Stream.fill(len)(elem)
    }

    if (isStreamBuilder(bf)) asThat(loop(len, this))
    else super.padTo(len, elem)(bf)
  }

  override def reverse: Stream[A] = {
    var result: Stream[A] = Stream.Empty
    var these = this
    while (!these.isEmpty) {
      val r = cons(these.head, future(result))
      r.tail // force it!
      result = r
      these = these.tail
    }
    result
  }

  override def flatten[B](implicit asTraversable: A => GenTraversableOnce[B]): Stream[B] = {
    def flatten1(t: Traversable[B]): Stream[B] =
      if (!t.isEmpty)
        cons(t.head, future(flatten1(t.tail)))
      else
        tail.flatten

    if (isEmpty) Stream.empty
    else flatten1(asTraversable(head).seq.toTraversable)
  }

  override def view = new scala.collection.immutable.StreamView[A, Stream[A]] {
    override def force[B >: A, That](implicit bf: CanBuildFrom[Stream[A], B, That]) = Stream(this.iterator.toStream).asInstanceOf[That]
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
  }

  override def stringPrefix = "Stream"
}

final class StreamIterator[+A] extends Object with Iterator[A] {
  def this(self: Stream[A]) {
    this()
    cur = self
  }

  private[this] var cur: Stream[A] = _

  def hasNext: Boolean = cur.nonEmpty
  def next(): A =
    if (isEmpty) Iterator.empty.next
    else {
      val result = cur.head
      cur = cur.tail
      result
    }
  override def toStream = {
    val result = cur.toStream
    cur = Stream.empty
    result
  }
  override def toList   = toStream.toList
}

object Stream extends SeqFactory[Stream] {

  class StreamCanBuildFrom[A] extends GenericCanBuildFrom[A]

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Stream[A]] = new StreamCanBuildFrom[A]

  def newBuilder[A]: Builder[A, Stream[A]] = new StreamBuilder[A]

  import scala.collection.{Iterable, Seq, IndexedSeq}

  class StreamBuilder[A] extends scala.collection.mutable.LazyBuilder[A, Stream[A]] {
    def result: Stream[A] = apply(parts.toStream flatMap (_.toStream))
  }

  case object Empty extends Stream[Nothing] {
    override def isEmpty = true
    override def head = throw new NoSuchElementException("head of empty stream")
    override def tail = throw new UnsupportedOperationException("tail of empty stream")
    def tailDefined = false
  }

  override def empty[A]: Stream[A] = Empty

  override def apply[A](xs: A*): Stream[A] = apply(xs.toStream)

  def apply[A](s: scala.Stream[A]): Stream[A] = s match {
    case scala.Stream.cons(head, tail) => cons(head, future(apply(tail)))
    case scala.Stream.Empty => Empty
  }

  def unapply[A](s: Stream[A]): Option[scala.Stream[A]] = Some(if (s.isEmpty) scala.Stream.Empty else scala.Stream.cons(s.head, unapply(s.tail).get))

  implicit class ConsWrapper[A](tl: Future[Stream[A]]) {
    def #::(hd: A): Stream[A] = cons(hd, tl)
    def #:::(prefix: Stream[A]): Stream[A] = prefix append tl
  }

  val #:: = Cons

  case class Cons[+A](hd: A, tl: Future[Stream[A]]) extends Stream[A] {
    private[this] var defined: Boolean = _
    override def isEmpty = false
    override def head = hd
    def tailDefined = defined
    override def tail: Stream[A] = {
      defined = true
      Await.result(tl, Duration.Inf)
    }
  }

  def iterate[A](start: A)(f: A => A): Stream[A] = apply(scala.Stream.iterate(start)(f))

  override def iterate[A](start: A, len: Int)(f: A => A): Stream[A] = iterate(start)(f) take len

  def from(start: Int, step: Int): Stream[Int] = apply(scala.Stream.from(start, step))

  def from(start: Int): Stream[Int] = from(start, 1)

  def continually[A](elem: => A): Stream[A] = apply(scala.Stream.continually(elem))

  override def fill[A](n: Int)(elem: => A): Stream[A] = apply(scala.Stream.fill(n)(elem))

  override def tabulate[A](n: Int)(f: Int => A): Stream[A] = apply(scala.Stream.tabulate(n)(f))

  override def range[T: Integral](start: T, end: T, step: T): Stream[T] = apply(scala.Stream.range(start, end, step))
}
