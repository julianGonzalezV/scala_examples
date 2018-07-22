package co.com.progfun.chapter4

object EitherMain extends App{

  /**
    * Note como el resultado de la función lo colocamos a que sea un Either en donde por ellado
    * izquierdo ante un error se retorna unstring y por el lado derecho el valor de la operación,
    * en este caso un Double.
    * @param xs
    * @return
    */
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)


  /**
    * O si prefiere el stack trace completo coloque el exception completo
    * @param x
    * @param y
    * @return
    */
  def safeDiv(x: Double, y: Double): Either[Exception, Double] = Try(x / y)


  def Try[A](a: => A): Either[Exception, A] =
    try {
      Right(a)
    } catch {
      case e: Exception => Left(e)
    }




  //Pruebas
  println(mean(IndexedSeq(3,6,9)))
  println(safeDiv(9,0))
  //Note como el error anterior no para el computo y sigue con la división de 9/3, fascinante no?
  println(safeDiv(9,3))


  println("For comprehension:::::::::::::::")
  //Note como el computo de lEmployee da como resultado un Left("invalid name")
  //porque generó error en la creación del modelo
  val vForEitherBad = for {
    age <- Right(42)
    name <- Left("invalid name")
    salary <- Right(1000000.0)
  } yield Employee(name, age, salary)
  println(vForEitherBad)

  val vForEitherOk = for {
    age <- Right(42)
    name <- Right("juligove")
    salary <- Right(1000000.0)
  } yield Employee(name, age, salary)
  println(vForEitherOk)

}
