package co.com.progfun.chapter4

object HandlingErrors {

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


  def mean(xs: Seq[Double]): Double ={
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length
  }



  def main(args: Array[String]): Unit ={

    //println(HandlingErrors.failingFn(12))
    println("No es Referential Transparency")
    println(HandlingErrors.failingFnRT(12))

    println(HandlingErrors.mean(Seq(3,6,9)))
    println(HandlingErrors.mean(Nil))




  }

}
