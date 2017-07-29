// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen

object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  def sqr(n: Int): Int = n*n

  private def formatAbs(x: Int) =
    s"The absolute value of $x is ${abs (x)}"

  private def formatSqr(x: Int) =
    s"The square root of $x is ${sqr (x)}"

  val magic :Int = 42
  var result :Option[Int] = None

  def main(args: Array[String]): Unit = {
    assert (magic - 84 == magic.-(84))
    println (formatAbs (magic-100))

    assert (magic*magic == sqr(magic))
    println (formatSqr (magic))
  }
}
