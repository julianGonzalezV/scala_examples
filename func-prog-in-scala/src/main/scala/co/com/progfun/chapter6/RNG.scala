package co.com.progfun.chapter6

trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG)

  def double(rng: RNG): (Double, RNG)

  def intDouble(rng: RNG): ((Int,Double), RNG)

  def doubleInt(rng: RNG): ((Double,Int), RNG)

  def double3(rng: RNG): ((Double,Double,Double), RNG)
}

case class SimpleRNG(seed:Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
      (n, nextRNG)
  }

  override def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val v1: (Int, RNG) = rng.nextInt
    (math.abs(v1._1), v1._2)
  }

  override def double(rng: RNG) = {
    val (result, nextState) = nonNegativeInt(rng)
    (math.abs(result)/(Int.MaxValue.toDouble+0.1), nextState)
  }

  override def intDouble(rng: RNG): ((Int,Double), RNG) = {

  }

  override def doubleInt(rng: RNG) = ???

  override def double3(rng: RNG) = ???
}

case class JulianoRNG(seed:Long) extends RNG {
  def nextInt: (Int, JulianoRNG) = {
    val newSeed = (seed * 2)
    val nextRNG = JulianoRNG(newSeed)
    val n = (newSeed + 2).toInt
    (n, nextRNG)
  }

  override def nonNegativeInt(rng: RNG) = ???

  override def double(rng: RNG) = ???

  override def intDouble(rng: RNG) = ???

  override def doubleInt(rng: RNG) = ???

  override def double3(rng: RNG) = ???
}