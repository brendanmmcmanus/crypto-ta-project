package Analysis

import scala.io.Source

object CSVReader {
  def readTimeSeriesFromCSV(filename: String): List[(String, Double)] = {
    var timesSeries = List[(String, Double)]()
    val file = Source.fromFile(filename)
    for (line <- file.getLines) {
      val data = line.split(",").map(_.trim)
      timesSeries ::= (data(0), data(1).toDouble)
    }
    timesSeries
  }
}