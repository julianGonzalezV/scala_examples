package co.com.progfun.miscellaneous.traitEg

import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

object Expansion {

def square(x:Double, exp:Int, acc: Double): Double = {
  if (exp <= 1) acc
  else square(x, exp-1, acc * x)
}

  def factorial(n: Int, acc:Double): Double = {
      if (n <= 1) acc
      else factorial(n-1, acc * n)
  }


  def expansion(x: Double):Double = {
    def loop(nIter:Double, acc:Double):Double = {
      if(nIter <= 1) {
        println("va a retornar")
        acc + x + 1
      }
      else {
        val squa = square(x, nIter.toInt, x)
        val facto = factorial(nIter.toInt, 1)
        val result = (squa / facto)
        println("sq=> "+squa)
        println("factorial=> "+facto)
        println("result=> "+result)
        loop(nIter-1, acc+result)
      }
    }
    val v1: Double = loop(x,0)
    val v2: String = f"$v1%1.4f"
    v2.replace(",", ".").toDouble

  }

  def main(args: Array[String]) {
    /*
    val stdin = scala.io.StdIn

    val n = stdin.readLine.trim.toInt


    for (nItr <- 1 to n) {
      val x = stdin.readLine.trim.toDouble
    }*/

    val v1 = expansion(4.0)
    println(":::Test1 ::::")
    println(v1)


  }
}

