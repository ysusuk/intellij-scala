import scalaz._, Scalaz._
object Demo extends App {
  def queryNextNumber: Exception \/ Long = {
    val source = Math.round(Math.random() * 100)
    if(source <= 60) \/.<ref>right(source)
    else \/.<ref>left(new Exception("Number is too big!"))
  }
  println(queryNextNumber)
}