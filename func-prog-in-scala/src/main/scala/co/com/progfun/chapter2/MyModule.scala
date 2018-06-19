package co.com.progfun.chapter2

//Singleton Object
object MyModule {

  def abs(number: Int): Int = if (number > 0) number else -number

  private def formatAbs(x: Int) = {
    val msg = "the absolute value o %d id %d"
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


  def fibo(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else
      fibo(n - 1) + fibo(n - 2)
  }




  def fiboTailRec(n: Int ):Int ={

    def fiboIter(n: Int, acc): Int = {
      if (n <= 1) acc
      else
        fiboIter(n - 1, acc + n)
    }

    fiboIter(n, 0)

  }


  // el main es considerado un procedure o impure function
  def main(args: Array[String]): Unit ={
    println(formatAbs(-42))
    println("facto: "+factorial(5))
    println("fibo: "+fibo(7))
  }

}
