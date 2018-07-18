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

    /**
      *
      * @param f
      * @tparam B
      * @return
      */
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case _ => None
    }

    override def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse(None)

    override def getOrElse[B >: A](default: => B): B = get

    override def orElse[B >: A](ob: => Option[B]): Option[B] = Some(get)

    override def filter(f: A => Boolean): Option[A] = {
      if(f(get)) Some(get) else None
    }
}

  case object None extends Option[Nothing] {
    override def map[B](f: Nothing => B): Option[B] = None

    override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

    override def getOrElse[B >: Nothing](default: => B): B = default
    override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

    override def filter(f: Nothing => Boolean): Option[Nothing] = None
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


  def variance(xs: Seq[Double]): Option[Double] ={
    if (xs.isEmpty) None
    else{
      val m = meanOk(xs)
      //con mao sería Option[Some[Double]]
      //con flatMap como lo recomienda el libro es Option[Double], que es lo que retorna la funcion :)
      val v1: Option[Double] = m.flatMap(m=> Some(xs.map(x=> math.pow(x-m,2)/xs.length).sum))
    }

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

    println("Some test::::::::::::::::::")
    println(HandlingErrors.Some(5).map(x=> x +3))
    val someSome= Some(Some(3))
    println(someSome)
    println("Map::::::::::::::::::")
    println(someSome.map(x =>  x))
    println("FlatMap:lo aplana :O:::::::::::::::::")
    println(someSome.flatMap(x =>  x))
    println(HandlingErrors.meanOk(Nil))

    println("getOrElse::::::::::::::::::")
    println(Some(5).getOrElse(2))
    println(None.getOrElse(2))

    println("orElse::::::::::::::::::")
    println(Some(5).orElse(Some(2)))
    println(None.orElse(Some(2)))

    println("Employee exAMPLE::::::::::::::::::")
    case class Employee(name: String, department: String){
      def otroDepartamento():String ={
        println("entra a la funcion")
        "Tulua"
      }
    }
    val employeesByName: Map[String, Employee] =
      List(Employee("Alice", "R&D"), Employee("Bob", "Accounting")).
        map(e => (e.name, e)).toMap
    println(employeesByName)
    //note como ya el None se aplica acá, el .get de un Map devuelve un Option
    //note lo potente de no validar qque si employeesByName.get("Joe") != null etc etc
    val dept1: scala.Option[String] = employeesByName.get("Joe").map(_.department)
    println(dept1)
    val dept2: scala.Option[String] = employeesByName.get("Bob").map(_.department)
    println(dept2)

    //Este ejemplo es más bacano, mire como de una cuando no na nada, es decir None en
    //get("joe") NO EJECUTA EL MAP Y NI SIUQIERA LLAMA A otroDepartamento y no sacó Null Poiter Exception
    // Te suena familar :)
    val dept3: scala.Option[String] = employeesByName.get("Joe").map(_.otroDepartamento())
    println(dept3)
    val dept4: scala.Option[String] = employeesByName.get("Bob").map(_.otroDepartamento())
    println(dept4)

  }

}
