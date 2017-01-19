package co.com.chapter1.modules

import co.com.chapter1.adts.Common.Amount

import co.com.chapter1.adts.{Account, Balance}

import scala.util.{Failure, Success, Try}



trait AccountService {

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


  /*
    if (a.balance.amount < amount)
      Failure(new Exception("Insufficient balance in account"))
    else Success(a.copy(balance = Balance(a.balance.amount – amount)))

*/


  /**
    *
    * @param a
    * @param amount
    * @return
    */
  def credit(a: Account, amount: Amount): Try[Account] =
    Success(a.copy(balance = Balance(a.balance.amount + amount)))
}


object AccountService extends AccountService