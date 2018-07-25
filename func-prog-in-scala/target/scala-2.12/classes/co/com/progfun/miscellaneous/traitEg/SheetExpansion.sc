val lista = List(0.toDouble, 1.toDouble, 2.toDouble, 3.toDouble,
  4.toDouble, 5.toDouble, 6.toDouble, 7.toDouble, 8.toDouble, 9.toDouble)


val entrada: Double = 0.0000

var factorial: Function1[Double, Double] = new Function1[Double, Double] {
  def apply (x:Double): Double = {
    def loop(x: Double, acc: Double = 1): Double = {
      if (x<=1) acc
      else loop(x-1, x*acc)
    }
    loop(x)
  }
}

val res = lista.fold(0.toDouble)((x: Double, y: Double) =>
  x + (Math.pow(entrada, y) / factorial(y)) )

BigDecimal(res).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble