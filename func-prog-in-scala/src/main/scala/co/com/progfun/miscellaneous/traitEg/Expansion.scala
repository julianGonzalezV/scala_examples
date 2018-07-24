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

def exponential(x:Double, exp:Int): Double = {
  def loop(x:Double, exp:Int, acc: Double): Double = {
    println("exp "+exp + " x "+x)
    if (exp <= 0) 1
    else if(exp==1) acc
    else loop(x, exp-1, x * x)
  }
  loop(x, exp, 1)
}

  def factorial(n: Double): Double = {
    def loop(n: Double, acc:Double): Double = {
      println("facto "+n+ " x ")
      if (n <= 1) 1
      else loop(n-1, acc * n)
    }
    loop(n, 1)
  }


  def expansion(x: Double):Double = {
    def loop(nIter:Double, acc:Double):Double = {
      if(nIter <= 0) {
        println("va a retornar")
        acc
      }
      else {
        val squa = exponential(x, 10)
        val facto = factorial(x)
        val result = (squa / facto)
        println("sq=> "+squa)
        println("factorial=> "+facto)
        println("result=> "+result)
        loop(nIter-1, acc+result)
      }
    }
    val v1: Double = loop(10,0)
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

