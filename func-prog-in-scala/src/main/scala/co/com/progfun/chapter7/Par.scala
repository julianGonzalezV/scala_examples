package co.com.progfun.chapter7

import java.util.concurrent.TimeUnit


object Par {
  //crea un computo e inmediatemente resuelve a Par[A]
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  //a diferencia de unit acá envuelve "a"  para ser ejecutado concurrentemente
  //y es non-strict
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

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



