package parser.ast.expressions

import lexer.{NL, SYMBOL, TokenStream}

import scala.annotation.tailrec

abstract class LeftAssociativeExpressionParser(nextLevel: ExpressionParser, oper: SYMBOL)
  extends AssociativeExpressionParser(nextLevel, oper) {

  def parse(tokens: TokenStream): Expression = {
    val expr = nextLevel.parse(tokens)
    if (!hasOper(tokens)) {
      expr
    }
    else {
      build(consumeArgs(tokens, nextLevel,  List(expr)))
    }
  }

  @tailrec
  private[this] def consumeArgs(tokens: TokenStream, nextLevel: ExpressionParser, args: List[Expression]): List[Expression] = {
    if (!hasOper(tokens)) {
      args.reverse
    }
    else {
      consumeOper(tokens)
      tokens.consumeOptionals(NL)
      consumeArgs(tokens, nextLevel, nextLevel.parse(tokens) :: args)
    }
  }

}
