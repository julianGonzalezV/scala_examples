package co.com.progfun.chapter4

object HandlingErrors {


  sealed trait Option[+A]{
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B

    /**B >: A
      * It's needed to
convince Scala that it is still safe to declare Option[+A] as covariant in A
(which lets the compiler assume things like Option[Dog] is a subtype of
Option[Animal]).
      * @param ob
      * @tparam B
      * @return
      */
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
  }
  case class Some[+A](get: A) extends Option[A] {
    override def map[B](f: A => B): Option[B] = ???

    override def flatMap[B](f: A => Option[B]): Option[B] = ???

    override def getOrElse[B >: A](default: => B): B = ???

    override def orElse[B >: A](ob: => Option[B]): Option[B] = ???

    override def filter(f: A => Boolean): Option[A] = ???
}
  case object None extends Option[Nothing] {
    override def map[B](f: Nothing => B): Option[B] = ???

    override def flatMap[B](f: Nothing => Option[B]): Option[B] = ???

    override def getOrElse[B >: Nothing](default: => B): B = ???
    override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ???

    override def filter(f: Nothing => Boolean): Option[Nothing] = ???
}

  //la idea es gestion de errores sin lanzar excepciones


  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fallo!!")
    //de aca en adelante es código unreachable, por la excepcion de la linea anterior, esto es solo para efectos de
    //prueba
    try{
      val x = 42 + 5
      x+y
    }

    catch {case e: Exception => 43}
  }


  /**
    * Rt de referential transparency
    * quiere decir que si en la operacion x+y colocams en lugar de y su valor, osea "throw new Exception("fallo!!")"
    * entonces la función debería retornar el mismo resultado que failingFn y no es así
    * ya que entra al catch
    *
    * Además TAMBIEN depende del contexto en donde se ejecute, note como al fallar dentro del try hizo otra cosa y si falla entoro
    * try? R/ va a  ir a otro resultado
    * @param i
    * @return
    */
  def failingFnRT(i: Int): Int = {
    //de aca en adelante es código unreachable, por la excepcion de la linea anterior, esto es solo para efectos de
    //prueba
    try{
      val x = 42 + 5
      x + ((throw new Exception("fallo!!")):Int)
    }

    catch {case e: Exception => 43}
  }


  /**
    * Ejemplo manejando la excepción, esa no es la idea por que estamos lanzando errores, para que de afuera las
    * gestionen
    * @param xs
    * @return
    */
  def mean(xs: Seq[Double]): Double ={
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length
  }

  /**
    * Ejemplo solicitando en la firma el valor cuando sea null, APARTE DE QUE LOS callers deben saber que valor
    * enviar en ése instante, que pasa si deseo detener el computo o irme por otro camino
    * quizás aveces se pueda pero otras no, ejemplo que pasa si se le envía puros negativos y precisamante el
    * resultrado es -1?? muerte cierto :(
    * @param xs
    * @return
    */
  def mean2(xs: Seq[Double], whenEmpty: Double): Double ={
    if (xs.isEmpty)whenEmpty
    else xs.sum / xs.length
  }

  /**
    * Manera correcta , acá podemos hacer validaciones de cuando es un Some o None
    * @param xs
    * @return
    */
  def meanOk(xs: Seq[Double]): Option[Double] ={
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }



  def main(args: Array[String]): Unit ={

    //println(HandlingErrors.failingFn(12))
    println("No es Referential Transparency")
    println(HandlingErrors.failingFnRT(12))

    println(HandlingErrors.mean(Seq(3,6,9)))
    //al fallar y no hacer nada con la falla el resto en adelante no se ejecuta
    //descomente y vera :)
    //println(HandlingErrors.mean(Nil))

    println(HandlingErrors.mean2(Seq(3,6,9), -1))
    println(HandlingErrors.mean2(Nil, -1))

    println(HandlingErrors.meanOk(Seq(3,6,9)))
    println(HandlingErrors.meanOk(Nil))




  }

}
