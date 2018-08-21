package co.com.progfun.chapter7

object MainChapter7 {

  def sum(ints: IndexedSeq[Int]): Int ={
    if (ints.size <= 1) ints.headOption getOrElse  0
    else{
      val (left, right) = ints.splitAt(ints.length/2)
      sum(left) + sum(right)
    }
  }





  def main(args: Array[String]): Unit = {


    println(sum(IndexedSeq(1,2,3)))

  }


}

