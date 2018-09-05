package co.com.chapter1.repositories

/**
  * La interface Repository NO SABE de como es la naturaleza del persistence storage, puede ser NoSQL y Relacional.
  * Los services modelan casos de uso
  */
trait AccountRepository {
  /* descomentar cuando existan los Entities and Values object
  def query(accountNo: String): Option[Account]
  def query(criteria: Criteria[Account]): Seq[Account]
  def write(accounts: Seq[Account]): Boolean
  def delete(account: Account): Boolean*/
}
