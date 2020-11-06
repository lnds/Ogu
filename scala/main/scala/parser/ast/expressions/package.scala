package parser.ast

import exceptions.InvalidExpression
import lexer._
import parser.ast.expressions.functions.ForwardPipeFuncCallExpression

package object expressions {

  def funcCallEndToken(tokens:TokenStream) : Boolean =
    tokens.nextSymbol() match {
      case NL | INDENT | DEDENT | ASSIGN | DOLLAR | COMMA | LET | VAR | DO | THEN | ELSE |
           RPAREN | IN | RBRACKET | RCURLY | WHERE => true
      case pipe if pipe.isInstanceOf[PIPEOPER] => true
      case oper if oper.isInstanceOf[OPER] => true
      case decl if decl.isInstanceOf[DECL] => true
      case EOF => true
      case _ => false
    }

  def parsePipedOrBodyExpression(tokens:TokenStream): Expression = if (!tokens.peek(NL)) {
    ForwardPipeFuncCallExpression.parse(tokens)
  }
  else {
    tokens.consume(NL)
    BlockExpression.parse(tokens)
  }

  def parseListOfCommaSeparatedExpressions(tokens:TokenStream) : List[Expression] = {
    consumeListOfExpression(tokens, ParseExpr, List(ParseExpr.parse(tokens)))
  }

  def parseListOfCommaSeparatedPipedExpressions(tokens:TokenStream) : List[Expression] = {
    consumeListOfExpression(tokens, ForwardPipeFuncCallExpression, List(ForwardPipeFuncCallExpression.parse(tokens)))
  }

  private[this]
  def consumeListOfExpression(tokens: TokenStream, parser: ExpressionParser, expressions: List[Expression])
  : List[Expression] = {
    if (!tokens.peek(COMMA)) {
      expressions.reverse
    }
    else {
      tokens.consume(COMMA)
      tokens.consumeOptionals(NL)
      if (tokens.peek(COMMA)) {
        throw InvalidExpression(tokens.nextSymbol(), tokens.currentLine())
      }
      consumeListOfExpression(tokens, parser, parser.parse(tokens) :: expressions)
    }
  }


}
