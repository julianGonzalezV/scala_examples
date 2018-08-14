package co.com.progfun.chapter6

trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG)

  def double(rng: RNG): (Double, RNG)

  def intDouble(rng: RNG): ((Int,Double), RNG)

  def doubleInt(rng: RNG): ((Double,Int), RNG)

  def double3(rng: RNG): ((Double,Double,Double), RNG)

  def ints(count: Int)(rng: RNG): (List[Int], RNG)
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
    val (intV, nextState) = nonNegativeInt(rng)
    val (doubleVal, nexState) = double(rng)
    ((intV, doubleVal), nexState)
  }

  override def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((intV, doubleVal), nexState) = intDouble(rng)
    ((doubleVal, intV), nexState)
  }

  override def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, nexState1) = double(rng)
    val (double2, nexState2) = double(nexState1)
    val (double3, nexState3) = double(nexState2)

    ((double1, double2, double3), nexState3)

  }
  override def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(count: Int)(state: RNG)(result: List[Int]):  (List[Int], RNG) ={
      if(count<=0) (result, state)
      else{
        val (intV, nextState) = nonNegativeInt(state)
        loop(count-1)(nextState)( intV :: result)
      }
    }

    loop(count)(rng)(List.empty)
  }
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

  override def ints(count: Int)(rng: RNG) = ???
}