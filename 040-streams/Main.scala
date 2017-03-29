// Advanced Programming 2015
//
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

println (Stream.to(3).toList)
assert(Stream.to(3).toList == List(1, 2, 3))
println (Stream.to(3).take(2).toList)
assert(Stream.to(3).take(2).toList == List(1, 2))
println (Stream.to(3).drop(1).toList)
assert(Stream.to(4).drop(2).toList == List(3, 4))
println(Stream.to(10).takeWhile(_ < 4 ).toList)
assert(Stream.to(10).takeWhile( _ < 4).toList == List(1, 2, 3))
