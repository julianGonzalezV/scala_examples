package co.com.progfun.chapter1

case class Charge(cc: CreditCard, amount: Double) {

  def combine(other: Charge): Charge = {
    // un case class puede ser creado sin necesidad de la palabra new
    if(cc == other.cc) Charge(cc, amount + other.amount)
    else throw new Exception("Can not combine charges to different cards")
  }

  //coalesce means join
  //este codigo es REUTILIZABLE, para lo que se necesite ejemplo en pruebas y no tiene side effec
  //No hemos llamado aún a un servicio del banco
  def coalesce(charges: List[Charge]): List[Charge] = {
    //los mortales hacemos:, esto es un map con la tarjeta y todos los cargos de esta
    val v1: Map[CreditCard, List[Charge]] = charges.groupBy(_.cc)
    //todos los  values del map (al parecer no importa el key) y los devuelve en el Iterable
    //Note que el value del map V1 es de tipo list por por lo tanto Iterable[Val] acá es un Iterable[List]
    val v2: Iterable[List[Charge]] = v1.values

    charges.groupBy(_.cc).values.map(_.reduce((ch1, ch2) => ch1.combine(ch2))).toList
  }
}
