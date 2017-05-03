import org.scalatest.{FreeSpec, Matchers, BeforeAndAfterAll}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.Dataset
import org.apache.spark.mllib.linalg.{Vector, Vectors}

class SentimentSpec extends FreeSpec with Matchers with BeforeAndAfterAll {

  org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
  org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)

  val spark =  SparkSession.builder
    .appName ("Sentiment")
    .master  ("local[12]")
    .getOrCreate

  override def afterAll = spark.stop

  import spark.implicits._

  val glove  = Main.loadGlove ("data/glove/glove.6B.50d.txt")

   "put your tests here" - {

    "nested tests group" - {
      "individual test #1" in {
        val reviews = Main.loadReviews ("data/Musical_Instruments_5_small.json")
        val words = Main.wordsSeperator(Main.tokenizer(reviews))
        assert(Main.features(words, glove).collect()(0).getDouble(1) == 2.0) // Negative
        assert(Main.features(words, glove).collect()(1).getDouble(1) == 2.0) // Positive
      }
      "individual test #2" in {
        val reviews = Main.loadReviews ("data/Musical_Instruments_5_small.json")
        val tokenized = Main.tokenizer(reviews).take(1).head._4.length
        val words = Main.wordsSeperator(Main.tokenizer(reviews))
        val word = words.groupBy("_1").count().filter($"_1" === 0).head.getLong(1)
        assert(tokenized == word)
      }
      "individual test #3" in {
        val reviews = Main.loadReviews ("data/Musical_Instruments_5_small.json")
        val glove = Main.loadGlove ("data/glove/glove.6B.50d.txt")
        val words = Main.wordsSeperator(Main.tokenizer(reviews))
        val average = Main.features(words, glove)
        val expected_features = Array((31, 2.0, Vectors.dense(0.1255215,0.28322693,-0.09646450000000001,0.1790677,0.328579,0.14379475000000003,-0.4609889000000001,-0.07140223999999999,0.04593803849999999,0.027005149999999995,0.031075499999999978,0.029017599999999998,-0.2824075,0.11188825000000002,0.277803425,0.25963250000000004,0.03502365,0.1528346,-0.364208455,-0.5585006,0.18188120000000002,0.26288645000000005,0.10764129999999998,-0.005248850000000008,0.20335835000000002,-1.495238,-0.3598839,0.24952902999999999,0.45840395,-0.17822079999999998,3.4493094999999996,0.24172725000000006,-0.11393490000000002,-0.20979199999999998,0.05814505649999999,0.006615165000000011,0.13158271500000002,0.2697706,0.1617977,-0.20023204999999997,0.029955849999999985,0.11593748,-0.12488065,0.28972565,-0.07222349999999998,0.07652325,-0.018566174999999997,0.15861755000000005,0.02197699999999999,0.21965479999999998)))
        val test_row = average.take(1)
        assert(test_row === expected_features)
      }
      "individual test #4" in { assert(true) }
      "individual test #5" in { assert(true) }
    }

  }

}
