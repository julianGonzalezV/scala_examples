package co.com.progfun.chapter6

trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG)

  def double(rng: RNG): (Double, RNG)

  def intDouble(rng: RNG): ((Int,Double), RNG)

  def doubleInt(rng: RNG): ((Double,Int), RNG)

  def double3(rng: RNG): ((Double,Double,Double), RNG)

  def ints(count: Int)(rng: RNG): (List[Int], RNG)

  type Rand[+A] = RNG => (A, RNG) //rand es un tipo de dato que recibe un A y retorna una tupla en donde la 1ra pos es del tipo
  //establecido y el segurndo es el un datoo de tipo RNG
  val int: Rand[Int] = _.nextInt  //igual que decir = x => x.nextInt, si llamaramos a double fallaría porque no cumplecon la firma
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] ={
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)


  /**
    * Funcion para combinar los resulados de dos Rands
    * @param ra
    * @param rb
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] ={
   rng =>   {
     val (a, rng1) = ra(rng)
     val (b, rng2) = rb(rng1)
     (f(a, b), rng2)
   }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]) = map2(ra, rb)((_, _))  // es lo mismo que decir map2(ra, rb)((x,y) => (x,y))


  val randIntDouble: Rand[(Int, Double)] =  both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =  both(double, int)


  def nonNegativeLessThanV1(n: Int): Rand[Int] = map(nonNegativeInt) { _ % n }

  /**
    * Problema: Devuelve un Rand de Any en ??? necesitamos pasarle yn rng pero no
    * lo tenemos
    * @param n
    * @return
    */
  def nonNegativeLessThanV2(n: Int): Rand[Any] = {
    map(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) mod else nonNegativeLessThan(n)(???)
  }

    //solucion al problema anterior
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }


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
    val (intV, nextState1) = nonNegativeInt(rng)
    val (doubleVal, nexState2) = double(nextState1)
    ((intV, doubleVal), nexState2)
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