package co.com.progfun.chapter4

import co.com.progfun.chapter5.{Cons, Empty}

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


  def mkName(name: String): Either[String, Name] ={
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
  }

  def mkAge(age: Int): Either[String, Age] ={
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))
  }

  /**
    * Pruebe enviando un dato ok y otro erróneo, pruebe enviando los 2 erróneos
    * Cómo puede mejorarse para que reporte ambos errores y no que cada vez que arrelglemos uno
    * salga el error del siguiente mal, imagine un formulario que no le diga de una sola
    * vez todos los errores que se tiene :( aburridor cierto?
    * @param name
    * @param age
    * @return
    */
  def mkPerson(name: String, age: Int): Either[String, Person] ={
    mkName(name).map2(mkAge(age))(Person(_, _))
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



  println("Mk Person example:::::::::")
  println("Ok =>  "+mkPerson("Juliancho", 31))
  println("one mistake  =>  "+mkPerson("ju", -10))
  //acá solo sale un error a la vez, muy aburridor para el caso en que existan varios
  //cómo lo mejoraría?, haría otro tipo de dato?
  println("Full mistake  =>  "+mkPerson("", -31))

  /*
  def cualquierFuncion(x:Int) = {println("cualquierFuncion"); x}
  def cualquierFuncion2(x:Int) = {println("cualquierFuncion"); x}
  val x = Cons(() => cualquierFuncion(x) , Cons(6, Empty) )
  println(x.headOption)
  println(x.headOption)*/




}
