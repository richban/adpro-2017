// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)

import org.apache.spark.ml.classification.MultilayerPerceptronClassifier
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.sql.{Dataset, Row, SparkSession}
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import scala.collection.parallel.immutable._

object Main {

	type Embedding       = (String, List[Double])
	type ParsedReview    = (Integer, String, Double)

	org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
	org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
	val spark =  SparkSession.builder
		.appName ("Sentiment")
		.master  ("local[9]")
		.getOrCreate
	val sc = spark.sparkContext

	import spark.implicits._

	val reviewSchema = StructType(Array(
			StructField ("reviewText", StringType, nullable=false),
			StructField ("overall",    DoubleType, nullable=false),
			StructField ("summary",    StringType, nullable=false)))

	// Read file and merge the text and summary into a single text column
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
			.withColumnRenamed ("_1", "word")
			.withColumnRenamed ("_2", "vec")
			.as[Embedding]

	// Split the review in words
	def tokenizer (reviews: Dataset[ParsedReview]) =
		new Tokenizer()
			.setInputCol("text").setOutputCol("words")
			.transform(reviews).as[(Integer, String, Double, Array[String])]

	def wordsSeperator (tokens: Dataset[(Integer, String, Double, Array[String])]) =
		tokens.flatMap { case (id, text, overall, words) =>
			words.map (
				word => ( id, if (overall > 3.0) 2.0 else if (overall == 3.0) 1.0 else 0.0, word )
		) }.withColumnRenamed("_3", "word")

	def features (words: Dataset[Row], glove: Dataset[Embedding]) = (
		   words.join(glove, words("word") === glove("word")).drop("word")
		  .withColumn("count", lit(1))
		  .as[(Integer, Double, Array[Double], Integer)])
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

	def perceptron (n: Int, splits: Array[Dataset[Row]], maxIter: Int) : Double =
	{
		val layers = Array[Int](50, 5, 4, 3)
		val trainer = new MultilayerPerceptronClassifier()
			.setLayers (layers)
			.setBlockSize (128)
			.setSeed (1234L)
			.setMaxIter (maxIter)
		val evaluator = new MulticlassClassificationEvaluator().setMetricName("accuracy")

		val train = splits.filter(m => m != n).reduce(_ union _)

		val test = splits(n)
		val model = trainer.fit(train)
		val result = model.transform(test)

		evaluator.evaluate (result.select("prediction", "label"))
	}

	def accuracy (glove: Dataset[Embedding], reviews: Dataset[ParsedReview]): List[Double] = {
		// Seperate all the words from the reviews
		val words = wordsSeperator(tokenizer(reviews))

		// Get the score for each word
		val average = features(words, glove)

		// Multilayer perceptron classifier
		val splits = average.randomSplit (
			Array(.1, .1, .1, .1, .1, .1, .1, .1, .1, .1), seed = 1234L)

		(0 to 9).par.map( x => perceptron(x, splits, 8/*Max iterations*/) ).toList
	}

	def run () = {
		val glove  = loadGlove ("data/glove/glove.6B.50d.txt")
		val reviews = loadReviews ("data/Musical_Instruments_5.json")		

		val accuracyList = accuracy(glove, reviews)

		val average_accuracy = accuracyList.reduce (_+_) / accuracyList.length
		val accurac_variance = accuracyList.foldLeft (0.0) (
			(acc, elem) => {
				acc + Math.pow (Math.abs (elem - average_accuracy), 2)
			}
		) / accuracyList.length

		println ("")
		println ("############################################")
		println ("### AVERAGE ACCURACY: " + average_accuracy + " ###")
		println ("### VARIANCE:   +/- " + accurac_variance + " ###")
		println ("############################################")
		println ("")

		spark.stop
	}

	def main(args: Array[String]) = run()

}
