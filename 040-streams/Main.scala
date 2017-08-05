// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

// this is how we do simple interactive testing

val l1 :Stream[Int] = Empty
val l2 :Stream[Int] = empty

val l3 :Stream[Int]= cons(1, cons(2, cons (3, empty)))

println (l1.headOption)
println (l2.headOption)
println (l3.headOption)

val naturals: Stream[Int] = from(1)
assert(Stream.to(3).toList == List(1,2,3))
println (naturals.take(1000000000).drop(41).take(10).toList)
assert( naturals.take(1000000000).drop(41).take(10).toList == List(42, 43, 44, 45, 46, 47, 48, 49, 50, 51))
println (naturals.takeWhile(_<1000000000).drop(100).take(50).toList)
println (naturals.forAll (_ < 0))
println (naturals.forAll2 (_ < 0))
// assert (naturals.forAll2 (_ > 0) == true)
println (naturals.takeWhile2(_<100000000).drop(100).take(50).toList)
println (l1.headOption2)
println (l2.headOption2)
println (l3.headOption2)
naturals.map (_*2).drop (30).take (50).toList
naturals.drop(42).filter (_%2 ==0).take (30).toList
println (fibs.take(100).toList)
fibViaUnfold.take(100).toList ==fibs.take(100).toList
from(1).take(1000000000).drop (41).take(10).toList == fromViaUnfold(1).take(1000000000).drop (41).take(10).toList
