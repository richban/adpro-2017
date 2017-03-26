// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Cons(h,t) => Some(h())
      case Empty => None
    }

  def tail :Stream[A] = this match {
      case Cons(h,t) => t()
      case Empty => Empty
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      case Empty => z
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      case Empty => z
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Cons (h,t) => p(h()) || t().exists (p)
      case Empty => false
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }

    // Exercise 4.2
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  //def find (p :A => Boolean) :Option[A] = this.filter (p).headOption
}




case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq

  // Exercise 4.1
  def to (n: Int) :Stream[Int] = {
    def reverse(acc: Int): Stream[Int] = {
      if (acc < n)
        cons(acc, reverse(acc + 1))
      else
        empty
    }

    reverse(1)
  }

  def from(n: Int) :Stream[Int] = {
    cons(n, from(n+1))
  }


}
// vim:tw=0:cc=80:nowrap
