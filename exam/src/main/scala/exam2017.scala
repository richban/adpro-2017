// Your name and ITU email: Richard Banyi riba@itu.dk
package adpro.exam2017

import scala.language.higherKinds
import fpinscala.monoids.Monoid
import fpinscala.monads.Functor
import fpinscala.state.State
import fpinscala.laziness.{Cons, Empty, Stream}
import fpinscala.laziness.Stream._
import fpinscala.parallelism.Par._
import adpro.data._
import adpro.data.FingerTree._
import monocle.Lens

import scalaz.Optional

object Q1 { 

  def hasKey[K,V] (l: List[(K,V)]) (k: K) :Boolean =
    l flatMap{
      t => t._1 map {key match {
        case k => true
        case _ => false
      }}
    }




  def reduceByKey[K,V] (l :List[(K,V)]) (ope: (V,V) => V) :List[(K,V)] =
    l foldLeft(0, List[(K, V)]()) ((a,c) => (c ++ c).groupBy(c._1).map(case (k, v) => (k, op(c._2, c._2))))

  def separate (l :List[(Int,List[String])]) :List[(Int,String)] =
    l flatMap { idws => idws._2 map { w => (idws._1,w) } }

  def separateViaFor (l :List[(Int,List[String])]) :List[(Int,String)] =
    l foldRight[List[(Int, List[String])]](empty)( (fa, fb)=> for {
      ba, bb <- fb
      a <- ba
  } yield (ba,bb) :: a)

} // Q1


object Q2 {

  trait TreeOfLists[+A]
  case object LeafOfLists  extends TreeOfLists[Nothing]
  case class BranchOfLists[+A] (
    data: List[A],
    left: TreeOfLists[A],
    right: TreeOfLists[A]
  ) extends TreeOfLists[A]

  trait TreeOfCollections[C[_]]
  case class LeafOfCollections () extends TreeOfCollections[Nothing]
  case class BranchOfCollections[A] (data: List[A],
                                     left: TreeOfCollections[A],
                                    right: TreeOfCollections[A],
                                    ) extends TreeOfCollections[A]

  def map[A,B] (t: TreeOfLists[A]) (f: A => B) :TreeOfLists[B] = t match {
    case LeafOfLists => LeafOfLists
    case BranchOfLists (data,left,right) =>
        BranchOfLists (data map f, map (left) (f), map (right) (f))
  }

  def map[A, B] (t: TreeOfCollections[A]) (f: A => B): C[B]

} // Q2

object Q3 {

  def p (n: Int): Int = { println (n.toString); n }

  def f (a: Int, b: Int): Int = if (a > 10) a else b

  // Answer the questions in comments here

  // A. 7, 42, 42

  // B. 42

  // C. nothing until the value of p is needed

} // Q3


object Q4 {

  sealed trait Input
  case object Coin extends Input
  case object Brew extends Input

  case class MachineState (ready: Boolean, coffee: Int, coins: Int)

  def step (i: Input) (s: MachineState) :MachineState =  ???

  def simulateMachine (initial: MachineState) (inputs: List[Input]) :(Int,Int) =  ???

} // Q4


object Q5 {

  def flatten[A] (s: =>Stream[List[A]]) :Stream[A] =

} // Q5


object Q6 {

  def parExists[A] (as: List[A]) (p: A => Boolean): Par[Boolean] = ???

} // Q6


object Q7 {

  def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, t: FingerTree[A]) :Z = ??? // assume that this is implemented
  def reduceR[A,Z] (opr: (A,Z) => Z) (t: FingerTree[A], z: Z) :Z = ??? // assume that this is implemented

  trait FingerTree[+A] {
  def addL[B >:A] (b: B) :FingerTree[B] = ??? // assume that this is implemented as in the paper
  def addR[B >:A] (b: B) :FingerTree[B] = ??? // assume that this is implemented as in the paper
  }

  def app3[A] (left: FingerTree[A], d: Digit[A], right: FingerTree[A]): FingerTree[A] = t match {
    case  app3(Empty (), ts, xs) => addL(ts, xs)
    case  app3(xs, ts, Empty ()) => addL(xs, ts)
    case  app3(Single(x), ts, xs) => addL(x, addL(ts, xs))
    case  app3(xs, ts, Single(x)) => addL((addL(xs,ts)), x)
    case  app3(Deep(pr1, m1, sf1), ts, Deep(pr2, m2, sf2)) =>
                  Deep(pr1, app3(m1, Node3(sf1, ts, pr2), m2), sf2)
  }

  // Implement this:

  def concatenate[A, B >: A] (left: FingerTree[A]) (right: FingerTree[B]) :FingerTree[B] =
    app3(left, Empty (), right)

} // Q7


object Q8 {

  getT (t: List[T]): Option[T] = t.map(x => Some(x))

  setT (f: T)(s: List[T]): List[T] = f::s

  def nullOption[T] = Optional[List[T], T] (getT)(setT)

  // Answer the questions below:

  // A. Yes because if we get some() and set it imediatly we end up with the same some

  // B. No because we can because we can return None

  // C. ...

} // Q8

