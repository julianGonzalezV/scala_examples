package co.com.progfun.chapter4

sealed trait Either[+E, +A]{

  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C]
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

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C) = ???
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

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C) = ???
}