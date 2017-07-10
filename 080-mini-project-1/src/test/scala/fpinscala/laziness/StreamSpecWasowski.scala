// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecWasowski extends FlatSpec with Checkers {

  import Stream._

  behavior of "headOption"

  // a scenario test:

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
    yield list2stream (la)


  val smallInt = Gen.choose(0, 10000)

  // a property test:

  it should "return the head of the stream packaged in Some (02)" in check {

    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } ) &&
    ("tail enforcing" |:
      Prop.forAll { (n: Int) => { cons(n, cons(throw new Error("Laziness tests"), Empty)).headOption; true } } )
  }

  behavior of "take"

  it should "take should not force any tails of the Stream it manipulates" in check {
    cons(9, cons(throw new Error("Laziness tests"), Empty)).take(1); true
  }

  it should "take should not force any heads of the Stream it manipulates" in check {
    cons(throw new Error("Laziness"), cons(throw new Error("Laziness tests"), Empty)).take(2); true
  }

  it should "take(n) does not force (n+1)st head ever (even if we force all elements of take(n))" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll { (s: Stream[Int]) => {
      val n = s.toList.length
      s.append(cons(throw new Error("Laziness tests"), Empty)).take(n)
      true
    } }
  }

  it should "s.take(n).take(n) == s.take(n) for any Stream s and any (idempotency)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll { (s: Stream[Int], n: Int) => {
      s.take(n).take(n).toList == s.take(n).toList
    } }
  }

  behavior of "drop"

  it should "s.drop(n).drop(m) == s.drop(n+m) for any n, m (additivity)" in check {
    Prop.forAll (genNonEmptyStream[Int], smallInt, smallInt)
    { (s: Stream[Int], n: Int, m: Int) => {
      s.drop(n).drop(m).toList == s.drop(n+m).toList
    } }
  }

  it should "s.drop(n) does not force any of the dropped elements heads" in check {
    cons(throw new Error("Laziness"), cons(throw new Error("Laziness tests"), Empty)).drop(1); true
  }

  it should "the above should hold even if we force some stuff in the tail" in check {
    cons(9, cons(throw new Error("Laziness tests"), Empty)).drop(1); true
  }

  behavior of "map"

  it should "x.map(id) == x (where id is the identity function)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll { (s: Stream[Int]) => {
      s.map(identity).toList == s.toList
    } }
  }

  it should "map terminates on infinite streams" in check {
    Prop.forAll { (n: Int) => { Stream.from(n).map(identity); true } }
  }

  behavior of "append"

  it should "it should not force tail or head of the appended stream" in check {
    Stream(1, 2, 3, 4).append(cons(throw new Error("Laziness tests"), Empty))
    true
  }

  it should "it should not change when append empty" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll { (s: Stream[Int]) => {
      s.append(Empty).toList == s.toList
    } }
  }

  it should "append terminates on infinite streams" in check {
    Prop.forAll { (n: Int) => { Stream.from(n).append(Stream.from(10)); true } }
  }

}
