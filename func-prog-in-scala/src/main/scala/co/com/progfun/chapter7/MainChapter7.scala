package co.com.progfun.chapter7

import co.com.progfun.chapter7.Par.Par

object MainChapter7 {

  /**
    * Version NO PARALELA
    * @param ints
    * @return
    */
  def sum(ints: IndexedSeq[Int]): Int ={
    if (ints.size <= 1) ints.headOption getOrElse  0
    else{
      //note co9mo el método splitAt IndexedSeq devuelve 2 IndexedSeq
      //val v1: (IndexedSeq[Int], IndexedSeq[Int]) = ints.splitAt(ints.length/2)
      val (left, right) = ints.splitAt(ints.length/2)

      sum(left) + sum(right)
    }
  }


  /*
  Una version paralela sería--->

  //Se crearía lafuncion unit que lo que haría es ejecutar el computo en una unidad de paralelismo que envuelve un valor
  def unit[A](a: => A): Par[A]:
  //obtiene el resultado de la computación paralela
  def get[A](a: Par[A]): A

  con lo anterior sum parallel quedaría:

   */

  /*
  def sumParaV1(ints: IndexedSeq[Int]): Int ={
    if (ints.size <= 1) ints.headOption getOrElse  0
    else{
      val (left, right) = ints.splitAt(ints.length/2)
      val sumL: Par[Int] = Par.unit(sumParaV1(left))
      val sumR: Par[Int] = Par.unit(sumParaV1(right))
      val v1 = (s:ExecutorService) => Par.run(s)(sumL)

      Par.run(sumL).get() + Par.run(sumR).get()
    }
  }
*/

  def main(args: Array[String]): Unit = {
    println(sum(IndexedSeq(1,2,3)))
  }


}

