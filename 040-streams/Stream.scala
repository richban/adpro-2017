// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }

  // Exercise 4.2
  def toList :List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case Empty => Nil
  }

  // Exercise 4.3
  def take (n: Int) :Stream[A] = this match {
    case Cons(h,t) if (n > 0) => cons(h(), t().take(n-1))
    case _ => empty
  }

  def drop (n: Int): Stream[A] = this match {
    case Cons(_,t) if (n > 0) => t().drop(n-1)
    case _ => this
  }

  // Exercise 4.4
  def takeWhile (p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // Exercise 4.5
  def forAll (p: A => Boolean) :Boolean = this match {
    case Cons(h,t) if p(h()) => t().forAll(p)
    case _ => false
  }

  def forAll2 (p: A => Boolean) :Boolean =
    foldRight(true)((h,t) => p(h) && t)

  // Exercise 4.6
  def takeWhile2 (p: A => Boolean) :Stream[A] =
    foldRight[Stream[A]](empty)((h,t) => if (p(h)) cons(h,t) else empty)

  // Exercise 4.7
  def headOption2: Option[A] =
    foldRight[Option[A]](None)((h,t) => Some(h))

  // Exercise 4.8
  def map[B] (p: A => B) :Stream[B] =
    foldRight[Stream[B]](empty)((h,t) => cons(p(h),t))

  def filter (p: A => Boolean) :Stream[A] =
    foldRight[Stream[A]](empty)((h,t) => if (p(h)) cons(h,t) else t)
  
  def append[B >: A] (that: Stream[B]) :Stream[B] =
    foldRight(that)((h,t) => cons(h, t))

  def flatMap[B] (p: A => Stream[B]) :Stream[B] =
    foldRight[Stream[B]](empty)((h,t) => p(h).append(t))

  // Exercise 4.9
  def find (p :A => Boolean) :Option[A] = this.filter (p).headOption

  // Exercise 4.9
  // def mapViaUnfold[B] (p: A => B): Stream[B] 
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

  // Exercise 4.11
  def unfold[A,S] (z: S) (f: S => Option[(A,S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  // Exercise 4.1
  def to (n: Int) :Stream[Int] = {
    def reverse(acc: Int) :Stream[Int] = {
      if (acc <= n) cons(acc, reverse(acc+1))
      else empty
    }
    reverse(1)
  }

  def from (n: Int) :Stream[Int] = {
    cons(n, from(n+1))
  }

  // Exercise 4.10
  def fibs: Stream[Int] = {
    def go(n: Int, nn: Int) :Stream[Int] = {
      cons(n, go(nn, n+nn))
    }
    go(0,1)
  }

  // Exercise 4.12
  def fibViaUnfold: Stream[Int] = 
    unfold((0,1)){ case (a,b) => Some(a, (b, a+b)) }

  def fromViaUnfold (n: Int): Stream[Int] = 
    unfold(n)(n => Some(n, n+1))
}

// vim:tw=0:cc=80:nowrap
