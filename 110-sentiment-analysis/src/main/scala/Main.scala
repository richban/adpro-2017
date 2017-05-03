// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)

import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import org.apache.spark.ml.feature.{RegexTokenizer, Tokenizer}

import scala.collection.mutable.WrappedArray
import org.apache.spark.sql.functions._
import org.apache.spark.sql
import org.apache.spark.ml.linalg.Vectors

import org.apache.spark.ml.classification.MultilayerPerceptronClassifier
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
import org.apache.spark.ml.Pipeline
import org.apache.spark.ml.tuning.{CrossValidator, ParamGridBuilder}
import org.apache.spark.sql.Row
import org.apache.spark.ml.linalg.Vector


object Main {

	type Embedding       = (String, List[Double])
	type ParsedReview    = (Integer, String, Double)
  type TokenizedReview = (Integer, String, Double, Array[String])
  type TokenizedVector = (Integer, Double, Array[Double], Integer)

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

	// Split the review in words
	def tokenizer (reviews: Dataset[ParsedReview]) =
		new Tokenizer().setInputCol("text").setOutputCol("words")
									 .transform(reviews).as[TokenizedReview]


	def wordsSeparator (tokens: Dataset[TokenizedReview]) =
		tokens.flatMap { case (id, text, overall, words) =>
				words.map ( 
					word => ( id, if (overall > 3.0) 2.0 else if (overall == 3.0) 1.0 else 0.0, word )
		) }.withColumnRenamed("_3", "word")


	def features (words: Dataset[Row], glove: Dataset[Embedding]) = (
		   words.join(glove, words("word") === glove("word")).drop("word")
			.withColumn("count", lit(1))
		  .as[TokenizedVector])
			.groupByKey(_._1)
			// Sum vectors
			.reduceGroups(
					(x, y) => (x._1, x._2,
										 x._3.zip(y._3).map(z => (z._1 + z._2)), 
										 x._4 + y._4))
		  .map(_._2)
		  // Average vectors
		  .map(x => (x._1, x._2, Vectors.dense(x._3.map( d => d / x._4 ))))
		  .withColumnRenamed("_1", "id")
		  .withColumnRenamed("_2", "label")
		  .withColumnRenamed("_3", "features")

  def main(args: Array[String]) = {

    val glove  = loadGlove ("data/glove/glove.6B.300d.txt") // FIXME
    val reviews = loadReviews ("data/Musical_Instruments_5.json") // FIXME

    val words = wordsSeparator(tokenizer(reviews))
    val average = features(words, glove)

    // specify layers for the neural network:
    val layers = Array[Int](300, 5, 4, 3)

    val trainer = new MultilayerPerceptronClassifier()
      .setLayers(layers)
      .setBlockSize(128)
      .setSeed(1234L)
      .setMaxIter(100)

    // Configure the ML Pipeline with stages: tokenizer, words, average
    val pipeline = new Pipeline()
      .setStages(Array(trainer))

    // Construct a grid of parameters to searchover
    // No parameter search
    val paramGrid = new ParamGridBuilder().build()

    val evaluator = new MulticlassClassificationEvaluator()
      .setMetricName("accuracy")
      .setLabelCol("label")
      .setPredictionCol("prediction")


    val cv = new CrossValidator()
      .setEstimator(pipeline)
      .setEvaluator(evaluator)
      .setEstimatorParamMaps(paramGrid)
      .setNumFolds(10)

    // val splits = average.randomSplit(Array(0.8, 0.2), seed = 1234L)
    // val train = splits(0)
    // val test = splits(1)


    val model = cv.fit(average)

    model.avgMetrics.foreach(println)

    /* Make predictions on test documents. cvModel uses the best model found (lrModel).
    model.transform(test)
      .select("id", "prediction")
      .collect()
      .foreach { case Row(id: Long, prediction: Double) =>
        println(s"($id) --> prediction=$prediction")
      }
    */

    spark.stop
  }

}
