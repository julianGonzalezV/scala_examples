package co.com.progfun.chapter1

class Payment {
  def charge(cc: CreditCard,  charge: Double): Unit ={
    //dificult to test, no queremos que en la prueba de verdad descuente :)
    println("do something, call the Bank service service etc")
  }

}
