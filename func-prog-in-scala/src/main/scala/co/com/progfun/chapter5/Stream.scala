package co.com.progfun.chapter5

sealed trait Stream[+A]{


  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t)=> Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h,t)=> h() :: t().toList
  }

  /**
    * Si n es mayor a cero entonces se crea un nuevo cons con el head t a la cola se le aplica take de n-1
    * así para Stream('a','b','c') y n=2
    * iteracion 1 Stream(a) + Stream('b','c') take(1)
    * iteracion 2 Steam(a, b) + Stream('c') take(0)
    * iteracion 3 Steam(a, b) + Stream.empty (por que cuando es cero o menor de una retorna empty)
    * @param n
    * @return
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) => {
      if(n<=0) Stream.empty
      else Stream.cons(h(), t().take(n-1))
    }
    case _ => {
      Stream.empty
    }
  }



  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => {
      if(p(h())) Stream.cons(h(), t().takeWhile(p))
      else t().takeWhile(p)
    }
    case _ => {
      Stream.empty
    }
  }


  def drop(n: Int): Stream[A] = this match {
    case Cons(x, xs) => {
      if(n<=0) this
      else xs().drop(n-1)
    }
    case _ => Empty
  }

  override def toString: String = this match {
    case Empty => ""
    case Cons(h,t)=> h() + t().toString
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




  /**
    * Note como el apply que es como el constructor dice que use el smart constructor (funcion con el nombre de la clase ne minuscula)
    * @param as
    * @tparam A
    * @return
    */
  def apply[A](as: A*): Stream[A] ={
   // println("::Apply::")
    if (as.isEmpty) empty  else cons(as.head, apply(as.tail: _*))
  }



}