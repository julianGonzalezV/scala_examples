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


  /**
    * Que devuelve random? R Un númnero , por lo cual lo que hacemos es  que ése numero lo devuelva en una tupla en elprimer elemento y
    * para calcular el siguiente estado de devulve el objeto que calcula el random en el segundo elemento de la tupla
    * A continuación el ejemplo:
    *
    *
    * En la vida real nos podemos encontrar con
    * class Foo {
private var s: FooState = ...
def bar: Bar
def baz: Int
}//en donde posiblemente las funciones bar o baz alteren en estado de var s

    por lo cual el patron que se aplica es el mismo que el aplicado en random(tupla en donde el primer elemento es (ver arriba))
    solución:
    trait Foo {
def bar: (Bar, Foo)
def baz: (Int, Foo)
}

    */

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


  println(":::::::::nonNegativeInt(rng: RNG):::::::::::::::::::")
  val rng2: RNG = SimpleRNG(10)
  val nn1 = rng2.nonNegativeInt(rng2)
  val nn2 = nn1._2.nonNegativeInt(nn1._2)
  println(nn1._1.toDouble)
  println(nn2)

  println(":::::::::double(rng: RNG):::::::::::::::::::")
  val db1 = rng2.double(rng2)
  val db2 = db1._2.double(db1._2)
  println(db1)
  println(db2)

  println(":::::::::intDouble(rng: RNG):::::::::::::::::::")
  println(rng2.intDouble(rng2))

  println(":::::::::DoubleInt(rng: RNG):::::::::::::::::::")
  println(rng2.doubleInt(rng2))

  println(":::::::::double3(rng: RNG): ((Double,Double,Double), RNG):::::::::::::::::::")
  println(rng2.double3(rng2))

  println("::::::::def ints(count: Int)(rng: RNG): (List[Int], RNG):::::::::::::::::::")
  println(rng2.ints(7)(rng2))
}
