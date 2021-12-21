import java.io.{StreamTokenizer, StringReader}
import scala.util.control.Breaks.{break, breakable}

object Main extends App {
  val code =
    """let hello != 23;
      |return x""".stripMargin

  val tokenizer= new StreamTokenizer(new StringReader(code))
  var currentToken = tokenizer.nextToken
  while (currentToken != StreamTokenizer.TT_EOF){
    breakable {
      tokenizer.ttype match {
        case StreamTokenizer.TT_WORD => println(tokenizer.sval)
        case StreamTokenizer.TT_NUMBER => println(tokenizer.nval.toInt.toString)
        case _ if (currentToken == '=') =>
          currentToken = tokenizer.nextToken()
          if (currentToken == "=") println("==")
          else {
            println("=")
            break
          }
        case _ if (currentToken == '!') =>
          currentToken = tokenizer.nextToken()
          if (currentToken == '=') println("!=")
          else {
            println("!")
            break
          }
        case _ => println(currentToken.toChar)
      }
      currentToken = tokenizer.nextToken()
    }
  }
  println("terminating")
}

