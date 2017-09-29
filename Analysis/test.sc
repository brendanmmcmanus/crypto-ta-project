import scala.io.Source

object CSVReader {
  def readTimeSeriesFromCSV(filename: String): List[(String, Double)] = {
    var timesSeries = List[(String, Double)]()
    val file = Source.fromFile(filename)
    for (line <- file.getLines){
      val data = line.split(",").map(_.trim)
      timesSeries ::= (data(0), data(1).toDouble)
    }
    timesSeries
  }
}

val test = CSVReader.readTimeSeriesFromCSV("C:\\Users\\Obasi Onuoha\\Documents\\BW\\cryptos\\test.csv")
println(test)

def movingAverage(series: List[Double], period: Int): List[Double] =
  List.fill(period - 1)(0.0) ++ ((series.sliding(period).map(_.sum).map(_ / period)))

val test2 = movingAverage(test.map(_._2), 4)

def volatility(series: List[Double], period: Int): List[Double] = {
  val avg = movingAverage(series, period)
  val temp = series.zip(avg).map{case (a, b) => math.pow(a - b, 2)}
  movingAverage(temp, period).map(math.sqrt(_))
}

def correlation(data: List[Double], prediction: List[Double], period: Int): List[Double] = {
  val avg = movingAverage(data, period)
  val tot = data.zip(avg).map{case (a, b) => math.pow(a - b, 2)}
  val res = data.zip(prediction).map{case (a, b) => math.pow(a - b, 2)}
  tot.zip(res).map{case (a, b) => 1 - (b/a)}
}

val a = List((1,2),(1,2),(1,2),(1,2))

a.map{case (a, b) => a+b}
volatility(test.map(_._2), 5)
