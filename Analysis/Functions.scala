package Analysis

def movingAverage(series: List[Double], period: Int): List[Double] =
List.fill(period - 1)(0.0) ++ ((series.sliding(period).map(_.sum).map(_ / period)))
  
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
