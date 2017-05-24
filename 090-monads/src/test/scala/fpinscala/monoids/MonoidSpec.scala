// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
// Example solution for scala exercises using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package fpinscala.monoids
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

object MonoidSpec extends Properties("Monoids..") {

  import Monoid._

  // Exercise 4 (intro to the exercise)

  def associative[A :Arbitrary] (m: Monoid[A]) :Prop =
    forAll { (a1: A, a2: A, a3: A) =>
      m.op(m.op(a1,a2), a3) == m.op(a1,m.op(a2,a3)) } :| "associativity"

  def unit[A :Arbitrary] (m :Monoid[A]) =
    forAll { (a :A) => m.op(a, m.zero) == a } :| "right unit" &&
    forAll { (a :A) => m.op(m.zero, a) == a } :| "left unit"

  def monoid[A :Arbitrary] (m :Monoid[A]) :Prop = associative (m) && unit (m)

  property ("stringMonoid is a monoid") = monoid (stringMonoid)

  // Exercise 4: test intAddition, intMultiplication, booleanOr,
  // booleanAnd and optionMonoid.

  property ("intAddition is a monoid") = monoid (intAddition)
  property ("intMultiplication is a monoid") = monoid (intMultiplication)
  property ("booleanOr is a monoid") = monoid (booleanOr)
  property ("booleanAnd is a monoid") = monoid (booleanAnd)
  property ("optionMonoid is a monoid") = monoid (optionMonoid[Int])

  // Exercise 5

  def homomorphism[A :Arbitrary,B :Arbitrary]
    (ma: Monoid[A]) (f: A => B) (mb: Monoid[B]) :Prop =
      forAll {(a1: A, a2: A) =>
        mb.op(f(a1), f(a2)) == f(ma.op(a1, a2)) }


  property ("stringMonoid and listMonoid[Char] are isomorphic") = homomorphism(stringMonoid)(x => x.toList)(listMonoid[Char])


  def isomorphism[A :Arbitrary, B :Arbitrary]
    (mb: Monoid[B]) (f1: B => A) (ma: Monoid[A]) (f2: A => B): Prop =
      forAll {(b1: B, b2: B, a1: A, a2: A) =>
        ma.op(f1(b1), f1(b2)) == f1(mb.op(b1, b2)) &&
        mb.op(f2(a1), f2(a2)) == f2(ma.op(a1, a2))
      }

  property ("stringMonoid and listMonoid[Char] are isomorphic") =  homomorphism(stringMonoid)(x => x.toList)(listMonoid[Char]) && 
     homomorphism(listMonoid[Char])(x => x.mkString)(stringMonoid)


  property ("stringMonoid and listMonoid[Char] are isomorphic") =
    isomorphism(stringMonoid)(x => x.toList)(listMonoid[Char])(x => x.mkString)
  // Exercise 6

  property ("booleanOr and booleanAnd are isomorphic") =
    isomorphism(booleanOr)(a => !a)(booleanAnd)(a => !a)

  // Exercise 7 (the testing part)

  property ("productMonoid is a monoid") = monoid (productMonoid(stringMonoid)(stringMonoid))
}
