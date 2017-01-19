package co.com.chapter1.adts

import java.util.Date

import co.com.chapter1.adts.Common.Amount

/**
  * Created by juligove on 2017/01/16.
  */

object Common{
  type Amount = BigDecimal
}



case class Balance(amount: Amount = 0)
case class Account(no: String, name: String,
                   dateOfOpening: Date, balance: Balance = Balance())


