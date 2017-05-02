// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)

import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import org.apache.spark.ml.feature.{RegexTokenizer, Tokenizer}

import scala.collection.mutable.WrappedArray
import org.apache.spark.sql.functions
import org.apache.spark.sql

object Main {

	type Embedding       = (String, List[Double])
	type ParsedReview    = (Integer, String, Double)
  type TokenizedReview = (Integer, Double, WrappedArray[String])
  type TokenizedVector = (Integer, Double, List[List[Double]])
  type ExplodeReview   = (Integer, Double, String)

	org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
	org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
	val spark =  SparkSession.builder
		.appName ("Sentiment")
		.master  ("local[9]")
		.getOrCreate

  import spark.implicits._

	val reviewSchema = StructType(Array(
			StructField ("reviewText", StringType, nullable=false),
			StructField ("overall",    DoubleType, nullable=false),
			StructField ("summary",    StringType, nullable=false)))

	// Read file and merge the text abd summary into a single text column

	def loadReviews (path: String): Dataset[ParsedReview] =
		spark
			.read
			.schema (reviewSchema)
			.json (path)
			.rdd
			.zipWithUniqueId
			.map[(Integer,String,Double)] { case (row,id) => (id.toInt, s"${row getString 2} ${row getString 0}", row getDouble 1) }
			.toDS
			.withColumnRenamed ("_1", "id" )
			.withColumnRenamed ("_2", "text")
			.withColumnRenamed ("_3", "overall")
			.as[ParsedReview]

  // Load the GLoVe embeddings file

  def loadGlove (path: String): Dataset[Embedding] =
		spark
			.read
			.text (path)
      .map  { _ getString 0 split " " }
      .map  (r => (r.head, r.tail.toList.map (_.toDouble))) // yuck!
			.withColumnRenamed ("_1", "word" )
			.withColumnRenamed ("_2", "vec")
			.as[Embedding]

  def tokenize (reviews: Dataset[ParsedReview]): Dataset[TokenizedReview] =
    new Tokenizer()
      .setInputCol("text")
      .setOutputCol("words")
      .transform(reviews)
      .drop("text")
      .as[TokenizedReview]

  def explodeReview (tokens: Dataset[TokenizedReview]): Dataset[ExplodeReview] =
    tokens.flatMap(
      r => {
        val (id, overall, words) = r
        words.map( w => (id, overall, w))
      }
      )
      .withColumnRenamed("_1", "id")
      .withColumnRenamed("_2", "overall")
      .withColumnRenamed("_3", "word")
      .as[ExplodeReview]

  def tokenizeVector (tokens: Dataset[ExplodeReview],
    glove: Dataset[Embedding]): Dataset[TokenizedVector] =
      tokens.join(glove, "word")
        .groupBy("id", "overall")
        .avg("vec")
        .drop("word")

  def main(args: Array[String]) = {

    val glove  = loadGlove ("/Users/richardbanyi/Developer/itu/2017-adpro/110-sentiment-analysis/data/glove/glove.6B.300d.txt") // FIXME
    val reviews = loadReviews ("/Users/richardbanyi/Developer/itu/2017-adpro/110-sentiment-analysis/data/Musical_Instruments_5.json") // FIXME

    // replace the following with the project code
    glove.show
    reviews.show

    val tokenized = tokenize (reviews)
    tokenized.show

    val explode = explodeReview(tokenized)
    explode.show

    spark.stop
  }

}
