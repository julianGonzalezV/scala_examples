package co.com.progfun.chapter3

object DataStructures {

  //trait(abstract interface ) es bueno para definir tipos de dstos como lo es este caso
  //sealed se usa para que no se pueda implementar desde otra parte fuera de este archivo
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A] (head:A, tail: List[A]) extends  List[A]


  //ojo que esta aproximacion es para ejemplo esto no es TAIL RECURSIVE
  object List{
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }

    def product(ints: List[Int]): Int = ints match {
      case Nil => 1
      //case (0 , _) => 0
      case Cons(head, tail) => head * sum(tail)
    }

    def tail [A](li: List[A]): List[A] = li match {
      case Nil => Nil
      case Cons(head, tail) => tail
    }

    def setHead[A](li: List[A], newVal: A): List[A] = li match {
      case Nil => Nil
      //case (0 , _) => 0
      case Cons(x, xs) =>  Cons(newVal, xs)
    }


    def drop[A](list: List[A], n: Int): List[A] = {
      if(n<0) list
      else drop(tail(list), n-1)
    }

    def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
      case Nil => Nil
      case Cons(x, xs) => {
        if(f(x))
          dropWhile(tail(xs),f)
        else
          dropWhile( Cons(x, xs),f)
      }
    }

    /**
      * Esta es considerada una variadic function porque puede aceptar cero o más valores
      * @param as
      * @tparam A
      * @return
      */
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))


  }
  def main(args: Array[String]): Unit ={

    val list1 = List(1,2,3,4,5)
    val list2 = List(3,4,5,6)

    println("::::::::::::::::: List Sum    ::::::::::")
    println(List.sum(list1))

    println("::::::::::::::::: List Product    ::::::::::")
    println(List.product(list1))

    println("::::::::::::::::: A little of patern matching    ::::::::::")
    val patterEg1 = list1 match {
      case Cons(x, Cons(2, Cons(4,_)))=> x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3,Cons(4,_)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    //Note como a pesa de que cumple el tercer case y el cuarto, scala toma el que primero haga match por eso la
    //respuesta es 3
    println(patterEg1)


    println("::::::::::::::::: Variadic functions   ::::::::::")
    println(List.apply(1,2,3,4,5))
    println(List.apply("bar", "foo"))

    println("::::::::::::::::: Tail and setHead functions   ::::::::::")
    println(List.tail(list1))
    println(List.setHead(List("foo", "bar"), "neo"))


    println("::::::::::::::::: Drop and DropWhile functions   ::::::::::")


  }
}