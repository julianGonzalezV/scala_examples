package co.com.chapter1

import java.util.Calendar

import co.com.chapter1.adts.{Account, Balance}
import co.com.chapter1.modules.AccountService


/**
  * Created by juligove on 2017/01/16.
  */




object  Main extends  App{

  def today = Calendar.getInstance.getTime
  val a = Account("a1", "John", today)
  a.balance == Balance(0)

  val b =  AccountService.credit(a,1000)

  val t1 = a.balance == Balance(80)
  val t2 = a.balance == Balance(0)
  val t3 = b.flatMap(AccountService.credit(_, 10))
  val t4 = b.flatMap(cuenta => AccountService.credit(cuenta, 10))
  val t5 = AccountService.credit(a,1000).flatMap(AccountService.credit(_, 10)).flatMap(AccountService.credit(_, 15))

  //la anterior linea es lo mismo que decir
  val t6 = for( accPlus100   <- AccountService.credit(a,1000);
                accPlus10 <- AccountService.credit(accPlus100,10);
                accPlus15 <- AccountService.credit(accPlus10,15)
              )yield accPlus15

  println("t1 es "+t1)
  println("t2 es "+t2)
  println("t3 es "+t3.get.balance.amount)
  println("t5 es "+t5.get.balance.amount)
  println("t6 es "+t6.get.balance.amount)
}
