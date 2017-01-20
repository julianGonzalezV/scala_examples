package co.com.chapter2.adts

import java.util.Date

import co.com.chapter2.adts.Common.Amount



/**
  * Created by juligove on 2017/01/16.
  */

object Common{
  type Amount = BigDecimal
  type DateRange = {val Initdate: Date
                    val endDate: Date}
}
case class Balance(amount: Amount = 0)

 trait Account {
  def no: String
  def name: String
   def balance: Balance
}
trait InterestBearingAccount extends Account {
  def rateOfInterest: BigDecimal
}

//Al componer de InterestBearingAccount solicita que se declare el  rateOfInterest
case class SavingsAccount(no: String,name: String , balance: Balance,rateOfInterest: BigDecimal ) extends InterestBearingAccount


case class MoneyMarketAccount(no: String,name: String ,balance: Balance,rateOfInterest: BigDecimal ) extends InterestBearingAccount






