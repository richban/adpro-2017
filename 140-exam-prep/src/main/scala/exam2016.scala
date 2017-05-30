
package adpro.exam2016
object Q1 {
   def checksumImp (in: String) :Int = {
    var result = 0
    for (c <- in.toList)
      result = (result + c.toInt) % 0xffff
    return result
  }

  def checksumFun (in :String) :Int =
    (in.toList).foldLeft(0)((z, a) => (a+z) % 0xffff)

  // Write your answer for Task 2 here.
}


object Q2 {
  import fpinscala.monads.Functor
  import scala.language.higherKinds

  // define a function which can operate on list instead just operating on A
  def onList[A] (f: A => A) :List[A] => List[A] =
    (a: List[A]) => a.map(f)

  def onCollection[C[_],A] (f: A => A)
    (implicit functorC: Functor[C]) :C[A] => C[A] =
      (fc: C[A]) => functorC.map(fc)(f)

}

object Q3 {

  import fpinscala.monoids.Monoid
  import scala.language.higherKinds

  def foldBack[A] (l :List[A]) (implicit M :Monoid[A]) :A =
    (M.zero::l).foldLeft(l.foldRight(M.zero)(M.op))(M.op)

}

object Q4 {

  type Computation[A] = A => Either[A,String]

  def run[A] (init: A) (progs: List[Computation[A]]) 
    : (A,List[String]) = progs.foldLeft((init, List[String]()))((a, c) => c(a._1) match {
    case Left(b) => (b, a._2)
    case Right(err) => (a._1, a._2 :+ err)
  })

}


object Q5 {

  sealed trait Tree[A]
  case class Branch[A] (l: () => Tree[A], a: A, r:() => Tree[A]) extends Tree[A]
  case class Leaf[A] (a: A) extends Tree[A]

  def multiply (t: Tree[Int]) :Int = ??? // Task 7.

  // Task 8. (answer below in a comment)

}
   
object Q6 {
   
  sealed trait Nat[+A]
  case object Zero extends Nat[Unit]
  case class Succ[A] (pred: A) extends Nat[A]

  val zero /* : ... */ = Zero            // Task 9.
  val one  /* : ... */ = Succ (zero)     // Task 9.
  val two  /* : ... */ = Succ (one)      // Task 9.


  // def plus2  (x : ... ) : ... = ???        // Task 10.

}
