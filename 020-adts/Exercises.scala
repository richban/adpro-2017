// Advanced Programming 2017,
// A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1: Richard Banyi
// AUTHOR2:
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// An ADT of Lists

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2

  def tail[A] (as: List[A]): List[A] = as match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  // Exercise 3

  def setHead[A] (as: List[A], newHead: A) : List[A] = as match {
    case Nil => Cons(newHead, Nil)
    case Cons(_, t) => Cons(newHead, t)
  }

  // Exercise 4

  def drop[A] (l: List[A], n: Int) : List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  // Exercise 5
  // still unclear

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }
  // Exercise 6

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // Exercise 7

  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }

  def length[A] (as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 8

  // def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B) : B = ...

  // Exercise 9

  // def sum (as : List[Int]) : Int = ...
  // def product (as :List[Int]) : Int = ...
  // def length1 (as :List[Int]) : Int = ...

  // Exercise 10

  // def reverse[A] (as :List[A]) :List[A] = ...

  // Exercise 11

  // def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = ...

  // def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B) : B = ...

  // Exercise 12

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  // def concat[A] (as: List[List[A]]) :List[A] = ..

  // Exercise 13

  // def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = ...

  // Exercise 14

  // def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = ...

  // Exercise 15

  // def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = ...

  // Exercise 16

  // def add (l: List[Int]) (r: List[Int]): List[Int] = ...

  // Exercise 17

  // def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = ...

  // Exercise 18

  // def hasSubsequence[A] (sup: List[A], sub: List[A]) :Boolean = ...

  // Exercise 19

  // def pascal (n :Int) : List[Int] = ...

  // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))

}
