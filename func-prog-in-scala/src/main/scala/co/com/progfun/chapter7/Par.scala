package co.com.progfun.chapter7

import java.util.concurrent.TimeUnit


object Par {
  //crea un computo e inmediatemente resuelve a Par[A]
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  //a diferencia de unit acá envuelve "a"  para ser ejecutado concurrentemente
  //y es non-strict
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
    * Cuando hago a => es porque "a" es el dato tipo A de la función que retorna asyncF
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))

  }
      type Par[A] = ExecutorService => Future[A]

      //V1: Se llamaba get: obtiene el resultado de la computación paralela
      //V2 se llama run para indicar que acá es donde realmente se implementa el paralelismo
      def run[A](s: ExecutorService)(a: Par[A]): Future[A] =a(s) // note como le pasa a "a" el ExecutorService

      /**
        * Combina el resultado de 2 operaciones paralelas , aplicando una funcion binaria(que recibe 2 parámetros)
        *
        * @param a
        * @param b
        * @param f
        * @tparam A
        * @return
        */
      def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
        (es: ExecutorService) => {
          val af: Future[A] = a(es)
          val bf: Future[B] = b(es)
          UnitFuture(f(af.get, bf.get))
        }
      }

      /**
        * La idea de esa función como ejemplo es que dada una lista de Ints envuelta en un Par
        * devolver otra lista ordenada y también envuelta en un Par , opciones de solucion
        * 1) Por medio de la función run (arriba) Par.run (hacer run del par) obtener la lista, ordenarla
        * y envolverla de nuevo en otro Par.Pero que si queremos evitar ejecutar run??(entonces vaya al punto 2)
        *
        * 2) Es precisamente mediante map2, así como se implemente a continuación. En donde ya que map2 permite la manipulación del
        * value of a Par, apicandole una función (ver la implementación de map2 arriba)
        * @param parList
        * @return
        */
      def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
        map2(parList, unit(()))((a, _) => a.sorted)


      /**
        * Lifting any function of type A => B
        * to become a function that takes Par[A] and returns Par[B]
        * @param pa
        * @param f
        * @tparam A
        * @tparam B
        * @return
        */
      def map[A,B](pa: Par[A])(f: A => B): Par[B] =
        map2(pa, unit(()))((a,_) => f(a))

      /**
        * Con el lifting anterior entonces podemos volver a escribir la función sortPar implementada anteriormente
        * @param parList
        * @return
        */
      def sortParV2(parList: Par[List[Int]]) = map(parList)(_.sorted)




      /**
        * Funcion ára combinar N computaciones paralelas
        * I) Remember that Par[A] is simply an alias for ExecutorService => Future[A]
        *
        * fbs es
        * @param ps
        * @param f
        * @tparam A
        * @tparam B
        * @return
        */
      def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
        //fbs es el val para indicar el la función aplicada a A para retornar el listado de Bs
        val fbs: List[Par[B]] = ps.map(asyncF(f))
        sequence(fbs)
      }


      /**
        * La implementación es la misma que la del chapter 4 con Option
        * HandlingErrors.scala
        * @param ps
        * @tparam A
        * @return
        */
      def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
        def loop[A](li: List[Par[A]], acc:  Par[List[A]]): Par[List[A]] ={
          if(li.isEmpty) acc
          else loop(li.tail, map2(acc, li.head)((x,y)=> x :+ y))
        }
        loop(ps, Par[List])
      }

      /**
        * Filtrar los elementos de una lista en paralelo
        * @param as
        * @param f
        * @tparam A
        * @return
        */
      def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
        val v1: List[A] = as.filter(elem => f(elem))
        Par(v1)
      }

      //a diferencia de unit acá marca "a"  para ser ejecutada concurrentemente
      /**
        * Esta es la implementación más sencilla pero:
        * el new Callable[A]{..} se bloquea hasta que la operación que
        * está dentro de {..} termine
        * @param a
        * @tparam A
        * @return
        */
      def fork[A](a: => Par[A]): Par[A] = {
        es =>
          es.submit(new Callable[A] {
            def call = a(es).get
          })
      }

}

private case class UnitFuture[A](get: A) extends Future[A] {
  def isDone = true
  def get(timeout: Long, units: TimeUnit) = get
  def isCancelled = false
  def cancel(evenIfRunning: Boolean): Boolean = false
}



