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

    def setHead[A](li: List[A], newVal: A): List[A] = newVal match {
      case Nil => Nil
      case x =>  Cons(x, li)
    }


    def drop[A](list: List[A], n: Int): List[A] = {
      if(n<0) list
      else drop(tail(list), n-1)
    }


    /*
    Esta version me saca error de Error:(48, 19) type mismatch;
 found   : v1.type (with underlying type A)
 required: A*/
    /*
            if( f(v1) )

    def dropWhile1[A](list: List[A], f: A => Boolean): List[A] =  {

        def dropWLoop[A](realList: List[A], acc:List[A]):List[A] = realList match {
          case Nil => acc
          case Cons(h, t) => {
            if( f(h) )
              dropWLoop(tail(t), acc)
            else
              dropWLoop(tail(t), setHead(acc, h))
          }
        }

        dropWLoop(list, Nil)
    }*/

    def dropWhile2[A](list: List[A], f: A => Boolean): List[A] =  {

      def dropWLoop[B](realList: List[B], acc:List[B], f: B => Boolean):List[B] = realList match {
        case Nil => {
          println("Case 1: "+realList)
          acc
        }
        case Cons(h, t) => {
          if( f(h)){
            println("If : "+realList+ " acc "+acc)

            dropWLoop(t, acc, f)
          }
          else{
            println("else : "+realList+ " acc "+acc)

            //dropWLoop(t, Cons(h, acc), f)
            Cons(h, dropWLoop(t,acc,f))
          }

        }
      }

      dropWLoop(list, Nil, f)
    }

    //Eliminando loop interno de dropWhile

    def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (f(x)) dropWhile(xs, f)
        else Cons(x, dropWhile(xs, f))
      }
    }

    /**
      * Curried version:
      * La firma queda algo así como nameFunction(xs)(f)
      * que se lee así:
      * nameFunction(xs) retorna una funcion que luego llamaremos con la funcion f
      * esto es muy bacano porque elimina el problema de tener que indicar el tipo de dato
      * cuando le pasamos funcionnes anominas (ver en el main el ejemplo del como es consumuiendo ambos dropWhile)
      *  Tambien curried lo podemos ver como convertir una función ff(a, b) que recibe 2 argumentos a una f
      *  que reciba un argumento f(a) y retorne otra funcion de 1 argumento para pasarle el 2do algo así como f(a) => g(?)
      *  y a g le pasamos b
      *
      *  En este caso  scala infiere más facilmente el tipo de izq a derecha y la primera función a la izquirda recibe un listy
      *  de A al ser por ejemplo de entero lo liga de una
      * @param list
      * @param f
      * @tparam A
      * @return
      */
    def dropWhileCurried[A](list: List[A]) (f: A => Boolean): List[A] = list match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (f(x)) dropWhile(xs, f)
        else Cons(x, dropWhile(xs, f))
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


    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil  => a2
      case  Cons(h, t) => Cons(h, append(t, a2))
    }


    /**
      * La clave de que se retorne un Cons(x, init(xs)) es que la recursividad queda deltro del Cons Inicial o de la
      * primera iteración, el problema que tiene init es que recorre todo a diferencia de append por ejemplo que solo recorre
      * a1  o drop que solo remueve hasta N elemlentos, o dropWhile que solo remueve mientras f(h) se cumpla
      * @param li
      * @tparam A
      * @return
      */
    def init[A](li: List[A]): List[A] = li match {
      case Nil  => Nil
      case Cons(x, Nil) => Nil
      case  Cons(h, t) => Cons(h, init(t))
    }


    //Generalizando SUM y PRODUCT
    /**
      * También es curried para una mejor inferencia de tipos
      *
      * Cuál es la idea?
      * R/ Sacar / Abstraer la operación (f) (en este caso suma y multiplicación, pero pueden cumplir muchas más)
      * y tambiém se abstrae el valor (z) en caso de ser una lista Vacía
      * @param lis
      * @param z
      * @param f
      * @tparam A
      * @tparam B
      * @return
      */
  def foldRight[A, B] (lis: List[A], z: B)(f: (A, B)=> B) : B = lis match {
      //valor ante un nulo
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t,z)(f))
    }

    /**
      * Al colocarle Int a la lista y al enviarla al foldRight en el primer par de parametros el compilador ya
      * sale inferir el tipo porque está Currificada(for better type inference)
      * Y si esta función solo suma enteros no sabe sumar otro tipo, tocaría implementar la versión para otros tipos
      * eje Strings etc
      * @param lis
      * @tparam A
      * @return
      */
    def sum2[A](lis: List[Int]) = {
      foldRight(lis, 0)((x , y ) => (x + y) )
    }



    def product2[A](lis: List[Int]) = {
      foldRight(lis, 1)((x , y ) => (x * y) )
    }

    /*
    EXERCISE 7: Can product implemented using foldRight immediately
      halt the recursion and return 0.0 if it encounters a 0.0?

      R/ Me imagino algo así como enviar una funcion de parada x=> x ==0
      */



    //EXERCISE 9

    def length[A](l:List[A]): Int =  {
        /* aso sería la implementacion sin usar foldr:

      case Nil  => 0
      case Cons(h, t) => 1 + length(t)

      Note como usando foldRight se le pasa la lista de tip A y un Int (el cero)
      por lo cual por curried entonces la segunda funcition es (A, Int) => ???
      en donde para ??? me pregunto debo trabajar con el A o con el Int R/ Con el Int ...
      y que debo hacer con el Int R/ Incrementarlo!
      Acá descubri que el B de  la  f:(A, B)=> B del foldRight es el ACUMULADOR
      QUIEN AÑ FINAL SE RETORNA!
      */

      foldRight(l, 0)((x, y) => 1 + y)

    }


  }
  def main(args: Array[String]): Unit ={

    val list1 = List(1,2,3,4,5)
    val list2 = List(3,4,5,6)
    val list3 = List(11,12,13)

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
    println(List.drop(List(12,13,14,15,16), 3))

    //borra todos los pares, dejando en la lista los impares :)
    println(List.dropWhile(List(2,3,4,5,6), (x: Int) =>  x % 2 == 0 ))

    println("::::::::::::::::: DropWhile Cirried functions   ::::::::::")

    println(List.dropWhileCurried(List(2,3,4,5,6)) (x =>  x % 2 == 0 ))
    println(List.dropWhileCurried(List(2,3,4,5,6)) (x =>  x % 2 == 0 ))

    println(":::::::::::::::::append functions   ::::::::::")
    println(List.append(list1,list3))

    println(":::::::::::::::::Init functions   ::::::::::")
    println(List.init(List(3,4,5,6)))


    println(":::::::::::::::::Sum2 y Produ2  generalizing to higher-order functions   ::::::::::")

    println(List.sum2(List(3,4,5,6)))
    println(List.product2(List(3,4,5,6)))

    println("::::::::::::::::: Chapter 3  EXERCISE 8   ::::::::::")
    /*
     foldRight[A, B] (lis: List[A], z: B)(f: (A, B)=> B) : B
    Viendo la firma del método, es este caso particuarl
    Esto quiere decir que al enviarle a foldRight los primeros dos argumentos list:List[Int](inferido)  y z: List[Int]
    osea que A = Int  y B=  List[Int] entonces la primera parte del curried devuelve una funcion que recibe dos parámetros
    A Y B y devuelve un List[Int] "(Int, List[Int]) => List[Int]"  por lo cual pasarle Cons(_,_) es completamente válido

    Pues su firma es: (el primero es un valor unico y el segundo es una lista, lo mismo que lo que espera la funcion devuelta
     por vfl1)
    Cons[+A] (head:A, tail: List[A])

      Ver exolicación en código

      Finalmente en cada iteraciión arma la lista cons  ej Cons(3,Cons(4,Cons(5,Cons(6,Nil))))
    */

    val vfl1: ((Int, List[Int]) => List[Int]) => List[Int] = List.foldRight(List(3,4,5,6), Nil: List[Int])
    val vfl2: List[Int] = List.foldRight(List(3,4,5,6), Nil: List[Int])(Cons(_,_))
    //note como partimos la funcion fold en vfl y vfl2

    val fold1V = List.foldRight(List(3,4,5,6), Nil: List[Int])(Cons(_,_))
    println(fold1V)



    println("::::::::::::::::: Chapter 3  EXERCISE 9   Compute the length of a list using foldRight. ::::::::::")
    println(List.length(List(3,4,5,6, 5, 5)))

  }
}