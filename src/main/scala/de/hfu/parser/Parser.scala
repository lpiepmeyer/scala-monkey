package de.hfu.parser

import de.hfu.lexer._

import scala.collection.mutable.ListBuffer

class Parser(val lexer: TokenIterator) {
  var errors: ListBuffer[String] = ListBuffer()
  var curToken:Token=null
  var peekToken  :Token =null

  def nextTokens(): Boolean = {
    if (!lexer.hasNext) return false
    val pair = lexer.next()
    curToken = pair._1
    peekToken = pair._2
    true
  }

  def parseLet() = {
    if (!nextTokens()) throw new RuntimeException
    (curToken, peekToken) match {
      case (identifier: IdentifierToken, AssignmentToken) if nextTokens() && nextTokens() =>
        val expression = parseExpression()
        LetStatement(identifier.literal, expression)
      case _ => throw new RuntimeException
    }
  }

  def parseReturn() = {
    if (!nextTokens()) throw new RuntimeException
    val expression = parseExpression()
    ReturnStatement(expression)
  }

  def parseExpressionStatement():ExpressionStatement= {
    if(peekToken!=SemicolonToken)throw new RuntimeException
    ExpressionStatement(parseExpression())
  }

  def parseExpression(): Expression = {
    if (peekToken!=SemicolonToken) throw new RuntimeException
    curToken match {
      case IntegerToken(literal) => IntegerLiteral(literal.toInt)
      case TrueToken => BoolLiteral(true)
      case FalseToken => BoolLiteral(false)
      case IdentifierToken(name) => Identifier(name)
    }
  }

  def addStatement(statements: ListBuffer[Statement], statement: Some[Statement]): Unit = {
    if (peekToken==SemicolonToken && statement.isDefined)
      statements.addOne(statement.get)
    else throw new RuntimeException
  }


  def parseProgram(): Program = {
    val result: ListBuffer[Statement] = ListBuffer()
    while (nextTokens()) {
      curToken match {
        case LetToken =>
          val statement = parseLet()
          addStatement(result, Some(statement))
        case ReturnToken =>
          val statement = parseReturn()
          addStatement(result, Some(statement))
        case _ =>
          val statement = parseExpressionStatement()
          addStatement(result, Some(statement))
      }
    }
    Program(result.toList)
  }

}

/*


func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		if !p.expectPeek(token.SEMICOLON) {
			return program
		}
		p.nextToken()
	}

	return program
}



func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t)
		return false
	}
}

*/