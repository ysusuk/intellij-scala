class HighlightingError {

  import scalaz._
  import scalaz.std.list.listMonoid
  import scalaz.syntax.applicative._

  private type M[A] = RWS[Unit, List[String], Int, A]
  private type MU   = M[Unit]

  val m: MU = ().point[M] <ref>whenM true

}