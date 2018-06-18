package co.com.progfun.chapter1

class Cafe {

  def buyCoffee(cc: CreditCard): Coffee ={
    val cup = new Coffee()
    //al modificar acá ya hay side effect
    cc.charge(cup.price)
    cup
  }

/*
aunque cambió de responsabilidad y podamos hacer Payment una interface para hacer ŕuebas de payment.charge o
usemos un framework mock, aún se ve el side effect . Además de que si una persona al comprar N cafes habría que llamar n
veces el servicio del banco al final :S , Sigue siendo una solución débil
 */
  def buyCoffeeV2(cc: CreditCard, payment: Payment): Coffee ={
    val cup = new Coffee()
    //al modificar acá ya hay side effect
    payment.charge(cc, cup.price)
    cup
  }

//Solución al problema en la V2
  def buyCoffeeV3(cc: CreditCard): (Coffee, Charge) ={
    val cup = new Coffee()
    //retorna una tupla un cafe y un cargo a la cuenta que contienen la cuenta como tal y el valor de ese cafe en especifico
    (cup, Charge(cc, cup.price))
  }

  // Note como si funciona
  def buyCoffees(cc:CreditCard , n:Int): (List[Coffee], Charge) = {

    val purchases: List[(Coffee, Charge)] = List.fill(n) (buyCoffeeV3(cc))
    val (coffess, charges ) = purchases.unzip
    //Note como la tupla anterios lo que hace es que devuelve 2 listas organizadas por el tipo del primer valor de la tupla
    // y otra lista con el tipo del segunfo valor de la tupla quedó  unificado
    val aux1:Seq[Coffee] = coffess
    val aux2: Seq[Charge] = charges
    //finalmete lo que hace es un reduce de la lista aplicando la funcion combine
    (coffess, charges.reduce((charge1, charge2) => charge1.combine(charge2)))

  }

}
