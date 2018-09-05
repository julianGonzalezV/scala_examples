package co.com.chapter2.modules

import co.com.chapter2.adts.Common.{Amount, DateRange}
import co.com.chapter2.adts.{Account, Balance, InterestBearingAccount}

import scala.util.{Failure, Success, Try}



trait AccountService {

  /*
  /**
    *
    * @param a
    * @param amount
    * @return
    */

  def debit(a: Account, amount: Amount): Try[Account] = {
    if (a.balance.amount < amount)
      Failure(new Exception("Insufficient balance in account"))
    else
      Success(a.copy(balance = Balance(a.balance.amount - amount)))
  }

  /**
    *
    * @param a
    * @param amount
    * @return
    */
  def credit(a: Account, amount: Amount): Try[Account] =
    Success(a.copy(balance = Balance(a.balance.amount + amount)))
*/



  /**
    *
    * @tparam A: PARAM Constrain to ensure that only InterestBearingAccount are passed
    * @return
    */
  def calculateInterest[A <: InterestBearingAccount]: A => BigDecimal = {a =>
    a.balance.amount *  a.rateOfInterest
  }

  /**
    *
    * @tparam A: Constrain to ensure that only InterestBearingAccount are passed
    * @return
    */
  def calculateInterest1[A <: InterestBearingAccount](a: A): BigDecimal = {
    a.balance.amount *  a.rateOfInterest
  }


  /**
    *
    * @return
    */
  def calculateInterest2: InterestBearingAccount => BigDecimal = { a =>
    a.balance.amount * a.rateOfInterest
  }


  /**
    * Apuntarle a estas soluciones es acercarse mas a lo funcional, y voy a poder hacer
    * composicion etc
    * @return
    */
  def deductTax: BigDecimal => BigDecimal = {interest =>
    if (interest < 1000)interest   else interest - 0.1 * interest
  }


  /**
    *
    * @param interest: input of the function
    * @return
    */
  def deductTax1(interest:BigDecimal): BigDecimal= {
    if (interest < 1000)
     interest
    else
      interest - 0.1 * interest
  }




}


object AccountService extends AccountService