// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
//
// Write ITU email addresses of both group members that contributed to
// the solution of the exercise (in lexicographic order).
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

// The extension of App allows writing statements at class top level (the so
// called default constructor). For App objects they will be executed as if they
// were placed in the main method in Java.

object Exercises extends App {

  // Exercise 3

  // def fib (n: Int) : Int = ...

  // some tests (uncomment, add more):

  // assert (fib (1) == 0)
  // ...

  // Exercise 4

  // A simple object describing a cost line; implemented imperatively, Java
  // style (this way until we learn more Scala)
  class Expense {

    // A constructor definition
    def this (tag :String, price :Int) = {
      this()
      this.tag = tag
      this.price = price
    }

    var tag   :String = "" // a tag line in the accounting system
    var price :Int    = 0 // the price is in cents
  }

  // computes the total of expenses in cents

  // def total (expenses: Array[Expense]) :Int = ...

  val testcase1 = Array[Expense](
    new Expense("Coffee", 450),
    new Expense("Cake", 350) )

  // assert (total (testcase1) == 800) // uncomment

  // Add one or two more tests
  // ...


  // Exercise 5

  // def isSorted[A] (as: Array[A], ordered: (A,A) =>  Boolean) :Boolean = ...

  // some tests (uncomment)

  // assert ( isSorted (Array(1,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  // assert (!isSorted (Array(6,2,3,4,5,6), (a: Int, b: Int)=> a <= b))
  // assert (!isSorted (Array(1,2,3,4,5,1), (a: Int, b: Int)=> a <= b))

  // add two tests with another type, for example an Array[String]

  // Exercise 6

  // def curry[A,B,C] (f: (A,B)=>C) : A => (B => C) = ...
  //
  // test if it type checks by currying isSorted automatically

  // def isSorted1[A]: Array[A] => ((A,A)=>Boolean) => Boolean = ...

  // Exercise 7

  // def uncurry[A,B,C] (f: A => B => C) : (A,B) => C =

  // def isSorted2[A] : (Array[A], (A,A) => Boolean) => Boolean = ...

  // Exercise 8

  // def compose[A,B,C] (f: B => C, g: A => B) : A => C = ...

}
