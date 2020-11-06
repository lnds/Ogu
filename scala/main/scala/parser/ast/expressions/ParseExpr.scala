package parser.ast.expressions

import lexer._
import parser.ast.expressions.control.ControlExpression
import parser.ast.expressions.functions.LambdaExpression
import parser.ast.expressions.vars.{BindDeclExpression, LetDeclExpression, VarDeclExpression}

object ParseExpr extends ExpressionParser {
  def parse(tokens: TokenStream): Expression = {
    tokens.nextSymbol() match {
      case LET => LetDeclExpression.parse(tokens)
      case VAR => VarDeclExpression.parse(tokens)
      case BIND => BindDeclExpression.parse(tokens)
      case ctl if ctl.isInstanceOf[CONTROL] => ControlExpression.parse(tokens)
      case _ => LambdaExpression.parse(tokens)
    }
  }

}
