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



  def nonNegativeEven: Rand[Int] ={
    map(nonNegativeInt(_))(i => i - i % 2)
  }



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


  /*
  Nota: no enrredarse acá con las llaves lo siguiente es lo mismo que map(nonNegativeInt) ( i=> i % n ) ya que map aplica
  el concepto de currying

  Parámetro nonNegativeInt: Note que nonNegativeInt devuelve un pair (Int, RNG) y map recibe en su primer parámetro
  una función  Rand[+A] que es igual a decir que espera "RNG => (A, RNG)", es decir algo que devuelva un pair (A,RNG)
  por eso nonNegativeInt cumple con la firma, es heavy pero ahí vamos
   */
  def nonNegativeLessThanV1(n: Int): Rand[Int] = map(nonNegativeInt(_)) { _ % n }


  /**
    * Problema: Devuelve un Rand de Any en ??? necesitamos pasarle yn rng, porque nonNegativeLessThan es de tipo rand entonces
    * es como tener realmente nonNegativeLessThan(n:Int)(RNG[A] => (A,RNG) .Pero no
    * lo tenemos solo tenemos un i que es de
    * @param n
    * @return
    */
  def nonNegativeLessThanV2(n: Int)  =
    map(nonNegativeInt(_)) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) mod else nonNegativeLessThan(n)(???)
    }

    //solucion al problema anterior
  /*
  No olvide que el tipo de retorno de esta función es un Rand[Int]  es decir un una fucnión del tipo
  RNG => (Int, RNG) porque así lo dice la implementación de Rand por eso inicia con rng =>
  Por ejemplo si fuera Int=> Int el retorno entonces rng abajo sería de tipo Int (dato de entrada)
   */
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng2) //en el libro dice rng pero yo creo que es rng2 para que siempre se llama
        //a la función con otro estado en lugar del mismo, pues siempre va a retornar el mismo
    /*si por algun motivo existen numeros mayores que el mayor multiplo de n, por ejemplo que
      Int.Max no sea divisible exactemente en en entonces podríapasar.
        En fin si llega a pasar entonces lo que se hace es volver a intentar generar un nuevo nonNegativeLessThan */
  }

  /*
  def nonNegativeLessThanFMap(n: Int)  = {
    rng => {
      val (i, rng2) = nonNegativeInt(rng)
      flatMap(rng)(rng2)
    }
  }*/


  /*
  f es = RNG => (A,RNG)
  g = A => RNG => (B,RNG)  es decir (x:A)(y:RNG):
   */
    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
      rng => {
        val (a, rng2) = f(rng)
        val (b, rng3) = g(a)(rng2)
        (b, rng3)
      }
    }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] ={
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }



}

case class SimpleRNG(seed:Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
      (n, nextRNG)
  }

  /**
    * Devuelve randoms siempre positivos
    * @param rng
    * @return
    */
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