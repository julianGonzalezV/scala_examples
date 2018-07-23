package co.com.progfun.chapter4

sealed trait Either[+E, +A]{

  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C]
  def sequence [E,A](es: List[Either[E,A]]): Either[E, List[A]]
  def traverse [E,A,B](as: List[A])(f: => Either[E,B]): Either[E, List[B]]
}

/**
  * Left es reservado para los casos de error
  * @param value
  * @tparam E
  */
case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: (Nothing) => B) = {
    println("entra al map de  Left")
    Left(value)
  }

  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]) = {
    println("entra al fMap Left")
    Left(value)
  }

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]) = b

  //ante un error pues devuelva un left :)
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C) = Left(value)

  override def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  override def traverse[E, A, B](as: List[A])(f: => Either[E, B]): Either[E, List[B]] = ???
}

/**
  * Por el lado derecho el valor de la operación
  * @param value
  * @tparam A
  */
case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: (A) => B) = {
    println("entra al map de  Right")
    Right(f(value))
  }


  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]) = {
    println("entra al fMap Right")
    f(value)
  }

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]) = Right(value)

  //this: es el Right que es precisamente esta clase o la instancia Right de esta clase,
  //b: es el otro Either (Right o Left) por eso se le permite hacer el map para obtener el dato que posee
  // De igual forma puede implementar esto usando for Comprehension
  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C) = {
    this.flatMap(x=> b.map(y=> f(x,y)))
  }

  /**
    * Convierte o hace el famoso tortugazo cambia de List de Either a Either de List
    * @param es
    * @tparam E
    * @tparam A
    * @return
    */
  override def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
???
    /*
    def loop(lis: List[Either[E, A]] , acc: Either[E, List[A]]): Either[E, List[A]] ={
      if(lis.isEmpty)acc
      else if(lis.head.equals(Left)) loop(List(), Either("",List()))
      else loop(lis.tail, map2(acc, li.head)((x,y)=> x :+ y))
    }

    loop(es, Right(List()))*/
  }

  /**
    * Aplica de map + sequence y queda de complejidad O(n), si lo usaramos por separado sería O(n exp 2)
    * @param as
    * @param f
    * @tparam E
    * @tparam A
    * @tparam B
    * @return
    */
  override def traverse[E, A, B](as: List[A])(f: => Either[E, B]): Either[E, List[B]] = ???
}