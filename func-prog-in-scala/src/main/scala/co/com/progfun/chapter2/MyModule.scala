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

    /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Solution
*/

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

    println("frepetition-->"+fRepetition(4, List(5,7)) )
    println("frepetition2-->"+fRepetition(4, List(5,7)) )
  }

}
