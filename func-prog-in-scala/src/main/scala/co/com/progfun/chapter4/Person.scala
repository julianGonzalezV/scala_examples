package co.com.progfun.chapter4

case class Person(name: Name, age: Age){

  override def toString: String = {
     "{Name:"+ name + ", Age:"+age+"}"
  }
}
sealed class Name(val value: String){
  override def toString: String = {
    value
  }
}
sealed class Age(val value: Int){
  override def toString: String = {
    value.toString
  }
}

