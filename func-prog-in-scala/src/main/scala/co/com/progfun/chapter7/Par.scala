package co.com.progfun.chapter7


trait Par[+A]{
  //crea un computo e inmediatemente resuelve a Par[A]
  def unit[A](a: A): Par[A]

  //a diferencia de unit acá envuelve "a"  para ser ejecutado concurrentemente
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  //V1: Se llamaba get: obtiene el resultado de la computación paralela
  //V2 se llama run para indicar que acá es don de realmente se implementa el paralelismo
  def run[A](a: Par[A]): A

  /**
    * Combina el resultado de 2 operaciones paralelas , aplicando una funcion binaria(que recibe 2 parámetros)
    * @param p1
    * @param p2
    * @param f
    * @tparam A
    * @return
    */
  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A,B) => C): Par[C]

  //a diferencia de unit acá marca "a"  para ser ejecutada concurrentemente
  def fork[A](a: => Par[A]): Par[A]
}
