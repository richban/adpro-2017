import java.util.concurrent._
import scala.language.implicitConversions

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

object Par {

  type Par[A] = ExecutorService => Future[A]
  def run[A] (s: ExecutorService) (a: Par[A]) : Future[A] = a(s)


  case class UnitFuture[A] (get: A) extends Future[A] {
    def isDone = true
    def get (timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel (evenIfRunning: Boolean) : Boolean = false
  }

  def unit[A] (a: A) :Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C] (a: Par[A], b: Par[B]) (f: (A,B) => C) : Par[C] =
    (es: ExecutorService) => {
      val af = a (es)
      val bf = b (es)
      UnitFuture (f(af.get, bf.get))
    }

  def fork[A] (a: => Par[A]) : Par[A] = es => es.submit(
    new Callable[A] { def call = a(es).get }
  )

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  // Exercise 1 (CB7.4)

  def asyncF[A,B] (f: A => B) : A => Par[B] =
    a => lazyUnit(f(a))

  // map is shown in the book

  def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
    map2 (pa,unit (())) ((a,_) => f(a))

  // Exercise 2 (CB7.5)
  //
  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
   p(e).get == p2(e).get

  def sequence[A] (ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft[Par[List[A]]](unit(List())) ((t, h) => map2(t, h)(_ :+ _))


  // Exercise 3 (CB7.6)

  //this is shown in the book:

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
     val fbs: List[Par[B]] = ps.map(asyncF(f))
     sequence(fbs)
  }

  //def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
  //  val fbs: List[Par[A]] = as.filter(asyncF(f))
  //  sequence(fbs)
  //  }


  // Exercise 4: implement map3 using map2
  // We need a flatMap - Par[Par[D]]
  def map3[A,B,C,D] (pa :Par[A], pb: Par[B], pc: Par[C]) (f: (A,B,C) => D) :Par[D]  =
    join(map2(pa, pb)((a, b) => (map(pc)(c => f(a, b, c)))))

  // shown in the book

  // def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  // Exercise 5 (CB7.11)

  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] =
    es => choices(n(es).get)(es)

  def choice[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =
    choiceN(map(cond)(b => if(b) 0 else 1))(List(t, f))

  // Exercise 6 (CB7.13)

  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] =
    es => choices(pa(es).get)(es)

  def choiceNviaChooser[A] (n: Par[Int]) (choices: List[Par[A]]) :Par[A] =
    chooser[Int, A](n)(n => choices(n))

  def choiceViaChooser[A] (cond: Par[Boolean]) (t: Par[A], f: Par[A]) : Par[A] =
    chooser[Boolean, A](cond)(c => if(c) t else f)

  // Exercise 7 (CB7.14)

  def join[A] (a : Par[Par[A]]) :Par[A] =
    es => a(es).get() (es)


  class ParOps[A](p: Par[A]) {

  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
}

object Main {
    def main(args:Array[String]):Unit = {

        val pool = Executors.newFixedThreadPool(4)

        // Exercise 1 tests

        val asyncModuleSeven = Par.asyncF[Int, Int] ((_ % 7))
        assert (Par.run (pool) (asyncModuleSeven (9)).get == 2)

        // Exercise 2 tests

        val oneToSixPar = List.tabulate (7) (Par.unit (_))
        val parOneToSix = Par.unit ((0 to 6).toList)
        assert (Par.equal (pool) (Par.sequence (oneToSixPar), parOneToSix))

        //// Exercise 3 tests

        //val filter = (n :Int) => n % 3 == 0
        //val ninety = (1 to 90).toList
        //val filteredNinety = ninety.filter (filter)
        //assert (Par.run (pool) (Par.parFilter (ninety) (filter)).get
        //        == filteredNinety)

        //// Exercise 4 tests

        val one = Par.unit (1)
        assert (Par.run (pool) (Par.map3 (one, one, one) (_ + _ + _)).get == 3)

        //// Exercise 5 tests

        val list = List.tabulate (9) (Par.unit (_))
        val index = 4
        assert (Par.equal (pool) (Par.choiceN (
                Par.unit (index)) (list), list (index)))

        val parTrue = Par.unit (true)
        val parFalse = Par.unit (false)
        assert (Par.equal (pool) (Par.choice (parTrue) (parTrue, parFalse), parTrue))
        assert (Par.equal (pool) (Par.choice (parFalse) (parTrue, parFalse), parFalse))

        //// Exercise 6 tests

        val source = 3
        val criterion = Par.unit (source)
        val choice = (n: Int) => Par.unit (n * 2)
        assert (Par.equal (pool) (Par.chooser (criterion) (choice), choice (source)))
        pool.shutdown()
    }
}
