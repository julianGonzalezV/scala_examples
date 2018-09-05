package co.com.chapter1.factories.v1_0
//Account es considerado aggregate root
trait Account {
  def  no: String
  def name: String
  /*Descomentar cuando vayan existiendo las entidades
  def bank: Bank
  def address: Address
  def dateOfOpening: Date,
  def dateOfClose: Option[Date]*/
  //..
}
case class CheckingAccount(
no: String,
name: String
/*
bank: Bank,
address: Address,
dateOfOpening: Date,
dateOfClose: Option[Date]*/
//..
) extends Account
case class SavingsAccount(
                           //..
                           rateOfInterest: BigDecimal,//..
                         ) extends Account
object Account {
  def apply(/* parameters */) = {
    // instantiate Checking, Savings or MoneyMarket account
    // depending on parameters
  }
}