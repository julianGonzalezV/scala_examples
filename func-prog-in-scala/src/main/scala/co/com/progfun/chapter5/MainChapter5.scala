package co.com.progfun.chapter5

object MainChapter5 extends App {

  //5.1 Strict and non-strict functions
  /**
    * non-strict: Hace referencia a que una funcion puede escoger NO EVALUAR uno o todos sus argumentos
    * Strict: Lo contrario como por ejemplo el ejemplo siguiente  List(1,2,3,4) se transforma de una a List(10,12,13,14)
    * y luego se le hace filter(en ambientes de alta data no es tan recomendado por el performance)
    */
 val l1 =  List(1,2,3,4) map (_ + 10) filter (_ % 2 == 0) map (_ * 3)
  println(l1)

  /**
    * Todo lo que hemos escrit del chapter 1 - 4 ha sido Strict, po defecto las funciones en la mayoría de los lenguajes es
    * así, ya que se requieren sus valores resueltos.
    */
  /**
    * Otro ejemplo de Strict
    * @param x
    * @return
    */
  def square(x: Double): Double = {
    println("x=> "+ x)
    x * x
  }

  //note como a la función ya le llegó el 42 y no 41 + 1, es porque es STRICT
  println(square(41+1))
  //Note como al enviarle un error de una se muere un no continúa con la siguiente operación
  //square(sys.error("failure"))
  println(square(41+3))

  /**
    * Ejemplos de non-stirctness    IF, || , && , recordemos que estos operadores o funciones no
    * evalúan todos sus argumentos :)
    */

  /**
    * Forma de escribir en scala Non-Strict functions  eje  " => A" nada antes del =>
    *
    * Note como al trtar de hacer print de onTrue y onFalse, que son nonStrrict parameters
    * de una se genera error porque no están resueltos, si se colocan  auqe sean strict () onTrue: A
    * funcioanría
    * @param cond
    * @param onTrue
    * @param onFalse
    * @tparam A
    * @return
    */
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A ={
    //println(onTrue)  Al hacer print de una se resuelve cuando se usa O SE REFERENCIA POR PRIMERA VEZ , por eso genera error
    //println("if2 false=> "+onFalse)
    if (cond) onTrue else onFalse
  }

  //Funcionan los prints porque ya los parámetros están evaluados
  def if21[A](cond: Boolean, onTrue: A, onFalse: A): A ={
    //println(onTrue)
    //println("if21 false=> "+onFalse)
    if (cond) onTrue else onFalse
  }


  val ifv1 = if2(false, sys.error("fail"), 3)
  //note como este si se ejecuta y devuelve 3 porque la excepcion que se le manda cuando True no se resuelve
  println("Non-Strict: "+ifv1)

  //Acá cómo si se reuelve el error (strict) entonce no se ejecuta la función
  /*val ifvFuncinaPrints = if21(false, sys.error("fail"), 3)
  println("Strict: "+ifvFuncinaPrints)*/




  def maybeTwice(b:Boolean, i: => Int):Int = {
    if(b) i+i+i else 0
  }

  /**
    * La idea de Lazy es que lo usemos cuando no queremos que el código se llame varias veces, y más una función que
    * siempre va a retornar lo mismo, imagine que j es una función que devuelve un valor único siempre o que es una función que
    * hace un computo considerable y llamarlo varias veces causará que se evalue de nuevo el computo, con lazy se arregla esto
    *
    * Lazy entoonces 1) Demora el computo hasta que la variable se referencie o se use, 2) Hace caché del resultado de la variable
    * así que al llamar varias veces no va a evaluar el computo de nuevo.

    *
    * @param b
    * @param i
    * @return
    */
  def maybeTwiceLazy(b:Boolean, i: => Int):Int = {
    lazy val j = i
    if(b) j+j+j else 0
  }

  val mt1 = maybeTwice(true, {println("hi"); 1+41})
  println("Lazy vals")
 // * Note como así se hagan varios j+j+j+j va a retornar un solo hi, porque es lazy !!
  val mt2 = maybeTwiceLazy(true, {println("hi"); 1+41})

  /**
    * Lo anterior es la diferencia de una función que toma sus valores by-name(Non-Stric, resuelve hasta el final) vs by-value(strict)
    */

  /**
    * Así mismo lo anterior es la base de Streams :)
    */


 def cualquierFuncion(x:Int):Int = {println("cualquierFuncion"); x}
 def cualquierFuncion2(x:Int):Stream[Int] = {println("cualquierFuncion2"); Stream.cons(x, Empty)}

 println("Streams Test 1 Forcing thunk (thunk es la forma  => A para escribir non-strict functions)")
  //acá estamos forzando a que se evalue siempre la funcion que se le pase con () => se hace eso
  // en este caso la firma del constructor lo reglamente y que podemos hacer para volverlo non-strict y que haga caché??

 val x1 = Cons(() => cualquierFuncion(5),() => cualquierFuncion2(6))
 println(x1.headOption)
 println(x1.headOption)

  /**
    * Note como al al ejecutar test1 y test2 solo aparece una sola vez en test 2 el Print , porque hizo caché del val :)
    */

//Acá  la respuesta a la pregunta anterior
  // se logra con smart constructor en este caso Stream.cons
 println("Streams Test 2 Cache version")
  //Note tambien como lo primero que se llama es el apply
  //cualquierFuncion es una función para demostrar que SI HACE CAHE
  //Pero se puede crear el Stram directamente con los raw values eg Stream( 7, 8, 9, Empty)
 val x2 = Stream( cualquierFuncion(7) , cualquierFuncion2(8), Stream(9, Empty) )
  println(x2)
 println(x2.headOption)
 println(x2.headOption)

  println("Streams TO-LIST")
  println(x2.toList)

  val x3 = Stream( "hola" , "Como estas", "te ves igual", Empty)
  println(x3.toList)

  println("::::::take::::::::::::::")
  val vx =
  println(x2.take(2).toList)

  println("::::::Drop::::::::::::::")
  println(x2.drop(2))

  println("::::::Take While::::::::::::::")
  val intStream = Stream(1,2,3,4,5,6)
  println(intStream.takeWhile(_>3).toList)
  println(intStream.takeWhile(_%2 == 0).toList)

  println("::::::5.3 Separating program description from evaluation::::::::::::::")


  println("::::::Take While Fold R::::::::::::::")
  println(intStream.takeWhileFoldR(_>3).toList)
  println(intStream.takeWhileFoldR(_%2 == 0).toList)

 println("::::::TheadOptionFoldR::::::::::::::")
 println(intStream.headOptionFoldR)
 println(Stream.empty.headOptionFoldR)



 println("::::::Infinite Streams::::::::::::::")
 val ones: Stream[Int] = Stream.cons(1, ones)
 def constante[A, B](a: A)(f: A=> A): Stream[A] = Stream.cons(f(a),constante(f(a))(f))

 //note lo potente de este ejempplo como no se muere porloop infinito sino que se evalua
 //lo que se requiere y ya por ser non-strict
 println(ones.take(3))
println(constante(7)(x => x).take(5))

 println("::::::Infinite Sum::::::::::::::")
 //def sumRange(n: Int): Stream[Int] = Stream.cons(n, sumRange(n+1))
 def sumRange(n: Int): Stream[Int] = constante(n)(x=> x+1)
 println(sumRange(4).take(5))

 println("::::::Infinite Fibonacci::::::::::::::")


 def fiboInfinite() ={
  def loop(fibo0:Int, fibo1: Int):Stream[Int] = {
   Stream.cons(fibo0, loop(fibo1, fibo0+fibo1))
  }
  loop(0,1)
 }

def aux1(n:Int) = {
 println("aux=>"+n)
 if(n<=2) 1
 else n
}
//recordanto tail recursive
 def factorial(n:Int):Int = {
  def loop(n:Int, acc: Int):Int ={
   if(n<=0) acc
   else{
    loop(n-1, acc * n)
   }
  }
  loop(n, 1)
 }

 println("facto=> "+factorial(4))

 def fibo(n:Int):Int = {
  println("entra "+n)
  def loop(n:Int, acc:Int):Int ={

   if(n<=0) acc
   else{
    loop(n-1, acc)+loop(n-2, acc)
   }
  }
  loop(n, 1)
 }

 /**
 println(fibo(0))
 println(fibo(1))
 println(fibo(2))*/

 println("fibo=> "+fibo(6))
 println(fiboInfinite().take(8))
}
