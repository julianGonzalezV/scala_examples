package co.com.progfun.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed:Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
      (n, nextRNG)
  }
}

case class JulianoRNG(seed:Long) extends RNG {
  def nextInt: (Int, JulianoRNG) = {
    val newSeed = (seed * 2)
    val nextRNG = JulianoRNG(newSeed)
    val n = (newSeed + 2).toInt
    (n, nextRNG)
  }
}