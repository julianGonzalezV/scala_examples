package co.com.progfun.chapter5

sealed trait Stream[+A]{
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t)=> Some(h())
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h:() => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] ={
    lazy val head = hd
    lazy val tail = tl
    Cons(()=> head, ()=> tail)
  }

  def apply[A](as: A*): Stream[A] ={
    if (as.isEmpty) empty  else cons(as.head, apply(as.tail: _*))
  }



}