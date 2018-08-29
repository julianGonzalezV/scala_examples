package co.com.progfun.chapter4

import scala.collection.immutable.Stream.Empty

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

    /**
      *Funcion que retorna otro Option, muy util cuando deseamos encadenar computos
      * @param ob
      * @tparam B
      * @return
      */
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
    * Además TAMBIEN depende del contexto en donde se ejecute, note como al fallar dentro del try hizo otra cosa y si falla en otro
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
      //en lugar de flatMap tambien se podría usar  map, donde el tipo sería Option[Some[Double]]
      //con flatMap como lo recomienda el libro es Option[Double], que es lo que retorna la funcion :)
      m.flatMap(m=> Some(xs.map(x=> math.pow(x-m,2)/xs.length).sum))
    }

  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f


  def abs0: Option[Double] => Option[Double] = lift(math.abs)

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {case e: Exception => None }
  }



  def validateInsuranceRateQuoteRequest(age: String, numberOfSpeedingTickets: String) : Option[Double]={
    val ageAux: Option[Int] = Try(age.toInt)
    val numberOfSpeedingTicketsAux: Option[Int] = Try(numberOfSpeedingTickets.toInt)

    /*Formas de llamar a insuranceRateQuote con los datos sin el wrapper Option
      1) FlatMap seguido de Map
      2) For Comprehension
      3) Implementar una funcion propia que reciba 2 Option y resuelvan a un Option ver map2
     */
    //1) ageAux.flatMap(age => numberOfSpeedingTicketsAux.map(ticks => insuranceRateQuote(age,ticks)) )
    /*2)
    for(
      ageI<-ageAux;
      tcks<-numberOfSpeedingTicketsAux
    )yield insuranceRateQuote(ageI, tcks)
   */

    // 3)
    map2(ageAux, numberOfSpeedingTicketsAux)(insuranceRateQuote)
  }

/*
  def map2[A, B, C] (a:Option[A], b:Option[B]) (f: (A,B)=> C): Option[C] = a match {
    case Some(x) => b match {
      case Some(y) => Some(f(x,y))
      case None => None
    }
    case None => None
  }*/

  /**
    * Version mejorada de map2, trate de no usar tanto pattern matching, cuando sean posibles otras opciones
    * como por ejemplo la que se muestra acá
    * @param a
    * @param b
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def map2[A, B, C] (a:Option[A], b:Option[B]) (f: (A,B)=> C): Option[C] = {
     a.flatMap(x => b.map(y => f(x,y)))
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    age/numberOfSpeedingTickets
  }

  def sequence[A](a:List[Option[A]]): Option[List[A]] = {

    def loop[B](li: List[Option[B]], acc:Option[List[B]]): Option[List[B]] =  {
      println("sequence  -> "+li)
      if(li.isEmpty)acc
      else if(li.head.equals(None)) loop(List(), None)
      else loop(li.tail, map2(acc, li.head)((x,y)=> x :+ y))
    }

    loop(a, Some(List()))
  }

  /**
    * mUY FACIL DE hacer si es con map y luego sequence pero esa no es  la idea porque sería
    * O(nexp2) su complejidad
    *
    * Note comoal final esta es una version mejorada de map + sequence y queda de complejidad O(n)
    *
    * @param a
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

    def loop(li: List[A], acc:Option[List[B]]): Option[List[B]] =  {
      println("traverse  -> "+li)
      if(li.isEmpty)acc
      else if(li.head.equals(None)) loop(List(), None)
      else {

        val transfVal = try f(li.head) catch {case e: Exception => None }
        loop(li.tail, map2(acc,transfVal)((x,y)=> x :+ y))
      }
    }

    loop(a, Some(List()))
  }


  /**
    * Esta funcio tiene un problema y es que recorre 2 veces el set de datos O(n exp 2)
    * el primero para recorrer la lista uy devolver List[Option[A]] y el segundo para devolver un Option[List[A]
    * @param a
    * @return
    */
  def parseInts(a: List[String]): Option[List[Int]] ={
    sequence(a map (iStr=> {
      println("parseInts item -> "+iStr)
      Try(iStr.toInt)
    }))
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

    println("Filter + getOrElse")
    val deptJoe1: scala.Option[String] = employeesByName.get("Joe").map(_.department)
    val deptJoe: String = employeesByName.get("Joe").map(_.department).filter(_ != "Accounting").getOrElse("Default Dept")
    val deptBob: String = employeesByName.get("Bob").map(_.department).filter(_ != "Accounting").getOrElse("Default Dept")
    println("deptJoe => "+deptJoe+"  deptBob => "+deptBob)

    println("Mean and variance")
    println(HandlingErrors.meanOk(Seq(2,4,6,8,10,12)))
    println(HandlingErrors.variance(Seq(2,4,6,8,10,12)))

    println("Option composition and lifting:::::::::::::LIFTING EG1")
    /*
    Note lo bacano que es el lifting, que nos ofrece la propiedad de llevar funciones que trabajan con otros tipos que no son options o que
    no estan envueltos en un option (O CUALQUIER OTRO TIPO QUE NECESITEMOS NO SOLO Option, en el capítulo 7 se muestra como
    se hace lift a Par[A] def map[A,B](pa: Par[A])(f: A => B): Par[B] )y las convierte a que si se puedan manejar con option, mire como no se modificó para nada
    mathabs de java sino que que le hizo lifting. Lo anterior puede aplicarse a cualquier función :)
     */
    println(HandlingErrors.abs0(Some(-3)))


    println(":::::::::::::validateInsuranceRateQuoteRequest:::::::::::::::::::::::::LIFTING EG2")
    //nOTE COMO RETORNA NONE POR LA j EN EL STRING
    println("validateInsuranceRateQuoteRequest: "+validateInsuranceRateQuoteRequest("3J0", "3"))

    //Y ADEMÁS NOTE QUE EL CORRER EL MAIN EL COMPUTO NO SE PARA Y CONTINÚA CON EStA LÍNEA(213)
    //es decrir que no se para por el error dela "J"en la línea 310
    println("validateInsuranceRateQuoteRequest: "+validateInsuranceRateQuoteRequest("30", "3"))

    println(":::::::::.sequence::::::::::::::::")
    println(sequence(List(Some(1), Some(2), Some(3))))
    println(sequence(List(Some(1), Some(3), None)))


    println(":::::::::.Parse several  Ints::::::::::::::::")
    println(parseInts(List("1", "2", "3")))
    println(parseInts(List("1", "2h", "3")))


    println(":::::::::.TRAVERSE:::::::::::::::")
    println(traverse(List("1", "2", "3"))((x:String)=> Some(x.toInt)))
    println(traverse(List("1", "2h", "3"))((x:String)=> Some(x.toInt)))
  }

}
