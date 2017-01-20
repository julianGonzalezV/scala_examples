package co.com.chapter2

import java.util.Calendar

import co.com.chapter2.adts.Common.Amount
import co.com.chapter2.adts.{Account, Balance, SavingsAccount}
import co.com.chapter2.modules.AccountService

import scala.util.Try


/**
  * Created by juligove on 2017/01/16.
  */

object  Chapter2Main extends  App{

  println("--*******************+Chapter2Main")

  val a1 = SavingsAccount("a-0001", "ibm", Balance(100000), 0.12)
  val a2 = SavingsAccount("a-0002", "google", Balance(2000000), 0.13)
  val a3 = SavingsAccount("a-0003", "chase", Balance(125000), 0.15)
  val accounts = List(a1, a2, a3)

  val acc1 = accounts.map(AccountService.calculateInterest)
  val acc2 = accounts.map(AccountService.calculateInterest2)
  val finalAccounts = accounts.map(AccountService.calculateInterest).map(AccountService.deductTax)


  val finalAccounts0 = accounts.map(AccountService.calculateInterest andThen AccountService.deductTax)

  /*
 Lo siguiente no va a funcionar porque andThen recibe es una funcion
 del tipo c => d
  */
  //val finalAccounts1 = accounts.map(AccountService.calculateInterest1 andThen AccountService.deductTax)
  //En este ejemplo calculateInterest2 retorna la funcion que es lo que recibe andThen
  val finalAccounts2 = accounts.map(AccountService.calculateInterest2 andThen AccountService.deductTax)
  println("--accounts"+accounts)
  println("--acc1"+acc1)
  println("--acc2"+acc2)
  println("--finalAccounts"+finalAccounts)
  println("--finalAccounts1"+finalAccounts0)
  println("--finalAccounts2"+finalAccounts2)

  /*
  val dateRange = ???
    List(s1, s2, s3).map(AccountService.calculateInterest(_, dateRange))
  List(s1, s2, s3).map(AccountService.calculateInterest(_, dateRange))
    .foldLeft(BigDecimal(0)((a, e) => e.map(_ + a).getOrElse(a))
  List(s1, s2, s3).map(AccountService.calculateInterest(_, dateRange))
    .filter(_.isSuccess)
  def getCurrencyBalance(a: Account): Try[Amount] = //..
  def getAccountFrom(no: String): Try[Account] = //..
  def calculateNetAssetValue(a: Account, balance: Amount): Try[Amount] = //…
  val result: Try[(Account, Amount)] = for {
    s <‐ getAccountFrom("a1")
           b <‐ getCurrencyBalance(s)
                  v <‐ calculateNetAssetValue(s, b)
                       if (v > 100000)
  } yield (s, v)
*/


}
