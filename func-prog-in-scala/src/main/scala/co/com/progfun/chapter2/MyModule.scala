package co.com.progfun.chapter2

//Singleton Object
object MyModule {

  def abs(number: Int): Int = if (number > 0) number else -number

  private def formatAbs(x: Int) = {
    val msg = "the absolute value o %d is %d"
    msg.format(x, abs(x))
  }


  def factorial(n: Int): Int = {
    def factorialIter(n: Int, acc: Int): Int = {
      if (n == 0) acc
      else
        factorialIter(n - 1, acc * n)
    }

    factorialIter(n, 1)
  }

  private def formatFactorial(x: Int) = {
    val msg = "the factorial value o %d is %d"
    msg.format(x, factorial(x))
  }


  //formatAbs y formatFactorial son identicos, es allí donde podemos generalizar
  def formatOperation(opName: String, x: Int, f: Int => Int )={
    val msg = "the %s value o %d is %d"
    msg.format(opName, x, f(x))
  }


  def fibo(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else
      fibo(n - 1) + fibo(n - 2)
  }



//tail recursive está muy duro de hacer no me funciona
  def fiboTailRec(n: Int ):Int ={

    def fiboIter(n: Int, acc:Int): Int = {
      println("entra con n = "+n + " y acc con "+ acc)
      if (n == 0) acc
      else if (n == 1)  acc+1
      else

        fiboIter(n - 1, acc+ n-1+n -2 )
    }

    fiboIter(n, 0)

  }

  /**
    * Retorna el primer index en array, donde el valor del index es igual al key dado
    * Version monomorfica
    * @param array
    * @param key
    * @return
    */
  def findFirst(ss: Array[String], key: String): Int ={

    def findIter(n:Int): Int ={
      if(n >= ss.length)
        -1
      else if(ss(n).equals(key))
        n
      else
        findIter(n+1)
    }

    findIter(0)

  }

  /**
    *
    * @param ds
    * @param key
    * @return
    */
  def binarySearch(ds: Array[Double], key: Double): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val d = ds(mid2)
        if (d == key) mid2
        else if (d > key) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, ds.length - 1)
  }


//Version que acepta cualquier tipo
  def findFirstPolymor [T] (ss: Array[T], f: T => Boolean): Int = {
    def findIter(n:Int): Int ={
      if(n >= ss.length)
        -1
      else if(f(ss(n)))
        n
      else
        findIter(n+1)
    }

    findIter(0)
  }


  def isSorted [A] (as: Array[A], orderedFunction: (A,A) => Boolean): Boolean ={
    def isSortedIter(n:Int, result: Boolean): Boolean ={
      //println("n= "+ n + "res= "+result+ "as.length "+as.length + "as(n) "+as(n)+" as(n+1)="+as(n+1))
      if(n >= as.length -1) result
      else if(orderedFunction(as(n), as(n+1)))
        isSortedIter(n+1, true)
      else
        false
    }

    isSortedIter(0,false)
  }


  def fRepetition(num:Int,arr:List[Int]):List[Int] ={
    arr.flatMap(x => {
      (1 to num).map(repetition => {
        println(x)
        x
      })
    })
  }

  def fRepetition2(num:Int,arr:List[Int]):List[Int] ={

    def iter [A](n: Int, value:A, acc: List[A]): List[A]  = {
      if (n <= 0) acc
      else iter(n-1, value, value :: acc )
    }
    arr.flatMap(x => iter(num, x, List.empty))
  }

  def f(arr:List[Int]):List[Int] = {
    arr.filter(x => arr.indexOf(x)% 2 != 0)

  }



  /**
    * :::partial application  --Tema de HOF:::
    * El nombre es porque la funcion f es aplicada  a algunos y no s todos los argumentos que se necesitan
    *
    *
    * @param a
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return el resultado es un HOF:  Debe retornar una funcion que de B resuelva a C, esto nos lleva a hacer:
    *         >1) Un pimer acercamiento o escritura es escribir una funcion que reciba algo de tipo B ((b: B) => ???)
    *         >2) Que es el ???...R/ es un tipo C...y dentro de la firma de myPartial1 hay algo que me resuelva o
    *         retorne un C? R/ Siii!  es f!! entonces el right hand side (despúes del =>) de (b:B).. es f que recibe un "a"
    *         que ya lo tenemos afuera en myPartial1 y un tipo "B" que es precisamente lo que implementamos en 1)
    *
    * COMO SE LEE ESTE ENRREDO?
    * Sí tengo una funcion (en este caso myPartial1) que recibe 2 argumentos(si no lo dude son "a" y "f") el primero de
    * tipo A y el segundo una función que necesita un Tipo A y B (dos argumentos) para producir un C
    * Podemos retornar una funcion que que solo necesita B (porque ya nos dieron un A) para devolver un C
    *
    */
  def myPartial1 [A, B, C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b)
  }


  /**
    * CURRYING: Convierte una función f de N argumentos a una funcion de 1 argumento
    * que parcialmente aplica f
    *
    * > Se usa como
    * 1) Type Inference
    *
    * @param args
    */
  def curry [A, B, C](f: (A,B) => C): A => (B =>C) = {
    (a: A) => (b: B) => f(a, b)
  }

  // Nota mental: A => (B =>C)  es igual a decir A => B =>C

  def uncurry [A, B, C](f: A => (B =>C) ): (A ,B) => C = {
    (a: A, b: B) => f(a)(b)
  }




  /**
    * FUNCTION COMPOSITION
    * provee como salida una función para que sea la entrada de otra funcion
    * @param args
    */
  def compose [A, B, C](f: B => C, g: A => B): A => C = {
    (a: A)  => f(g(a))
  }




  // el main es considerado un procedure o impure function
  def main(args: Array[String]): Unit ={



    val array1 = List(1,2,3, 4, 5, 6, 7)
    println(array1.filter(x => array1.indexOf(x)% 2 != 0).toList)

    val array2 = List(1,2,3, 4, 5, 6, 7)
    List.empty :+ 1
    println(array2.filter(x => array2.indexOf(x)% 2 != 0))

    println(formatAbs(-42))
    println(formatFactorial(5))

    println("::::::::::::::::: generalizando SOLO REQUIERE DE UNA SOLA FUNCION :)  ")
    println(formatOperation("absolute", -5, abs ))
    println(formatOperation("Factorial", 5, factorial ))
    println("::::::::::::::::: ")

    println("fibo: "+fibo(7))
    println("fiboTailRec: BAD result "+fiboTailRec(7))

    println("::::::::::::::::: init Polimorphic Functions  ::::::::::")
    println(findFirst( Array("Apple", "Banana", "Orange"), "Orange"))
    println(findFirstPolymor( Array(4, 5, 6),  (x:Int) => x > 5))

    println("::::::::::::::::: end Polimorphic Functions  ::::::::::")

    println("frepetition-->"+fRepetition(4, List(5,7)) )
    println("frepetition2-->"+fRepetition(4, List(5,7)) )

    println("::::::::::::::::: init isSorted Polimorphic  ::::::::::")
    println(isSorted( Array("Apple", "Banana567", "Orange45"), (x:String, y:String) => x.length < y.length))
    println(isSorted( Array(4, 6, 7,8,4),  (x:Int, y:Int) => x < y))

    println("::::::::::::::::: end isSorted Polimorphic   ::::::::::")


    println("::::::::::::::::: Init myPartial1 partial application   ::::::::::")

    val partial1 = myPartial1(3, (x:Int, y: String)=> x == y.length )
    println( " PartialTrue "+ partial1("ABC") )
    //acá podría pensarse en una funcion de base de datos que se llame insertar pero que solo se le
    //pasa el elemento a insertar y el se encarga de abrir y cerrar conexiones a base de datos???? diría qque SI :)
    println( " PartialFalse "+ partial1("ABCd") )

    println("::::::::::::::::: End myPartial1 partial application  ::::::::::")

    println("::::::::::::::::: Init CURRY   ::::::::::")
    val myCurry = curry((x:Int, y: String)=> x == y.length )
    println( myCurry(4) )
    println( myCurry(4)("ABCD") )
    println( myCurry(4)("ABC") )
    println("::::::::::::::::: End CURRY  ::::::::::")


    println("::::::::::::::::: Init UNCURRY   ::::::::::")
    val uncurry1: (Int, String) => Boolean = uncurry((x:Int) => (y: String) => x == y.length )
    println( uncurry1(4, "ABCD") )
    println( uncurry1(4, "ABCDe"))
    println("::::::::::::::::: End UNCURRY  ::::::::::")



    println("::::::::::::::::: Init COMPOSE   ::::::::::")
    //Note como con podemos crear una composición de fácil

    val funcion1 = (x:Int) => x.toString
    val funcion2 = (x: String) => x.length
    val toStrAndGetLenght  = compose( funcion2, funcion1)
    println( toStrAndGetLenght(100))
    println( toStrAndGetLenght(5300))

    println("::::::::::::::::: End COMPOSE  ::::::::::")

  }

}
