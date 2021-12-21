import de.hfu.lexer.TokenIterator

object Main extends App {
  val code =
    """let hello != 23;
      |return x""".stripMargin

 for(token<-new TokenIterator(code)){
   println(token)
 }
}

