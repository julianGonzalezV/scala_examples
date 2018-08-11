package co.com.progfun.chapter6

import scala.util.Random

/**
  * La idea es ver el uso de estratregias cuando tratamos con side effect
  */

object MainChapter6 extends App{

  /**
    * Note como cada vez que se llama a random el valor generado es diferente por eso
    * es que se considera que no posee transparencia referencial
    */
  val randomNumber = new Random()
  println(randomNumber.nextDouble)
  println(randomNumber.nextInt)
  println(randomNumber.nextInt(10))
  println(randomNumber.nextInt(10))

  def rollDie: Int = {
    val rng = new scala.util.Random
    rng.nextInt(6)
  }




  //Implementando mi proipio random generator, pero funcioalmente puro

  println("My own randomn generator")
  val rng: JulianoRNG = JulianoRNG(42)
  val rd1: (Int, RNG) = rng.nextInt
  println(rd1)
  val rd2 = rd1._2.nextInt
  println(rd2)

  //Además note como es referentially transparent porque al llamar varias veces el nextInt
  //de cualquiera de los val el resultado es el mismo y no como la clase util random de java que retorna valores difernets
  //y si tenemos una secuencia de hasta rdn y la ejecutamos de nuevo los valores siempre van a ser los mismos, siempre y cuando no
  // se cambie la semilla en la clase  JulianoRNG(seed:Long), al retornar siempre lo mismo es un api PURA
  println("Y es referentially transparent ")
  println(rd1._2.nextInt)
  println(rd1._2.nextInt)
  println(rd1._2.nextInt)



}
