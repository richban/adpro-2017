trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // Exercise 1 (CB 6.1)

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, g) => (Int.MaxValue, g)
    case (x, g) => (Math.abs(x), g)
  }

  // Exercise 2 (CB 6.2)
  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (x, g) => (((x-1).toDouble) / Int.MaxValue, g)
  }
  // Exercise 3 (CB 6.3)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val rng1 = nonNegativeInt(rng)
    val rng2 = double(rng1._2)
    ((rng1._1, rng2._1), rng2._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val rng1 = double(rng)
    val rng2 = nonNegativeInt(rng1._2)
    ((rng1._1, rng2._1), rng2._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val rng1 = double(rng)
    val rng2 = double(rng1._2)
    val rng3 = double(rng2._2)
    ((rng1._1, rng2._1, rng3._1), rng3._2)
  }

  // def boolean(rng: RNG): (Boolean, RNG) =
  //  rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 4 (CB 6.4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    nonNegativeInt(rng) match {
      case (x, g) if count > 1 => {
        val rng1 = ints(count-1)(g)
        (x::rng1._1, rng1._2)
      }
      case (x, g) => (x :: Nil, g)
  }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5)

  val _double: Rand[Double] = map(nonNegativeInt)(i => ((i-1).toDouble) / Int.MaxValue)

  // Exercise 6 (CB 6.6)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  // this is given in the book

  // def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
  //  map2(ra, rb)((_, _))

  // val randIntDouble: Rand[(Int, Double)] = both(int, double)

  // val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      def loop(rs: List[Rand[A]], acc: List[A], g: RNG): (List[A], RNG) = rs match {
        case Nil => (acc.reverse, g)
        case x :: xs =>
          val (a, rng2) = x(rng)
          loop(xs, a :: acc, rng2)
      }
    loop(fs, List.empty, rng)
  }

  // def _ints(count: Int): Rand[List[Int]] = ...

  // Exercise 8 (6.8)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt){
      i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0){
          rng => (mod, rng)
        }
        else {
          rng => nonNegativeLessThan(n)(rng)
        }
    }
  }

}

import State._

case class State[S, +A](run: S => (A, S)) {

  // Exercise 9 (6.10)

  // def map[B](f: A => B): State[S, B] = ...

  // def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = ...

  // def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => { ...
  // 

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 9 (6.10) continued

  // def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = ...
  //
  // This is given in the book:

  // def modify[S](f: S => S): State[S, Unit] = for {
  //   s <- get // Gets the current state and assigns it to `s`.
  //   _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  // } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


  def random_int :Rand[Int] =  State (_.nextInt)

  // Exercise 10

  // def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = ...

  // Exercise 11

  // val random_integers = ...
  //

}

object Tests extends App {

  println("EXERCISE 1")
  println(RNG.nonNegativeInt(RNG.Simple(4200))._1);
  println("----------")

  println("EXERCISE 2")
  println(RNG.double(RNG.Simple(4200))._1)
  println("----------")

  println("EXERCISE 3")
  println(RNG.intDouble(RNG.Simple(4200))._1)
  println(RNG.doubleInt(RNG.Simple(4200))._1)
  println(RNG.double3(RNG.Simple(4200))._1)
  println("----------")

  println("EXERCISE 4")
  println(RNG.ints(5)(RNG.Simple(4200))._1)
  println("----------")

  println("EXERCISE 5")
  println(RNG._double(RNG.Simple(4200))._1)
  println("----------")

}
