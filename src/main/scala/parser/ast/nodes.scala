package parser.ast

import parser.ast.expressions.{AssignableExpression, Expression, Identifier, LambdaArg}







case class IdIsType(id: String, cl: String) extends Expression

trait LetId
case class LetSimpleId(id:String) extends LetId
case class LetTupledId(ids:List[LetId]) extends LetId

trait Variable
case class LetVariable(id: LetId, value: Expression) extends Variable


trait LetDeclExprTrait extends Expression
case class LetDeclExpr(decls: List[Variable], inExpr: Option[Expression]) extends LetDeclExprTrait
case class VarDeclExpr(decls: List[Variable], inExpr: Option[Expression]) extends Expression
case class BindDeclExpr(decls: List[Variable], inExpr: Expression) extends Expression

trait LoopDeclVariable extends Variable
case class LoopVarDecl(id: String, initialValue: Expression) extends LoopDeclVariable
case class ForVarDeclIn(id: String, initialValue: Expression) extends LoopDeclVariable
case class ForVarDeclTupledIn(ids: List[String], initialValue: Expression) extends LoopDeclVariable


case class WhereGuard(guarExpr: Option[Expression], body: Expression)

trait WhereDef
case class WhereDefSimple(id: String, args: Option[List[Expression]], body: Expression) extends WhereDef
case class WhereDefTupled(idList: List[String], args: Option[List[Expression]], body: Expression) extends WhereDef
case class WhereDefWithGuards(id: String, args: Option[List[Expression]], guards: List[WhereGuard]) extends WhereDef
case class WhereDefTupledWithGuards(idList: List[String], args: Option[List[Expression]], guards: List[WhereGuard]) extends WhereDef
case class WhereBlock(whereDefs: List[WhereDef]) extends LangNode

case class DefArg(expression: Expression)
object DefOtherwiseArg extends DefArg(null)


class DefDecl(id: String) extends LangNode
case class MultiMethod(inner: Boolean, id: String, matches: List[DefArg], args: List[DefArg], body: Expression, whereBlock: Option[WhereBlock])
  extends DefDecl(id)

case class SimpleDefDecl(inner: Boolean, id: String, args: List[DefArg], body: Expression, whereBlock: Option[WhereBlock])
  extends DefDecl(id) {
  def patterMatching(): Boolean =
    args.exists {
      case DefArg(Identifier(_)) => false
      case _ => true
    }
}


case class MultiDefDecl(id: String, decls: List[SimpleDefDecl]) extends DefDecl(id) {
  def patternMatching(): Boolean = decls.exists(_.patterMatching())

  def args : List[String] = {
    val count = decls.map(_.args.size).max
    var ids = List.empty[String]
    decls.foreach(decl => decl.args.foreach {
      case DefArg(Identifier(name)) if !ids.contains(name) => ids = name :: ids
      case DefArg(IdIsType(name, _)) if !ids.contains(name) => ids = name :: ids
      case _ =>
    })
    var i = 0
    while (ids.size < count) {
      ids = s"arg_$i" :: ids
      i += 1
    }
    ids.reverse
  }
}


trait CallExpression extends Expression
case class FunctionCallExpression(func: Expression, args:List[Expression]) extends CallExpression






trait DefBodyGuardExpr
case class DefBodyGuardExpression(comp: Expression, body: Expression) extends DefBodyGuardExpr
case class DefBodyGuardOtherwiseExpression(body: Expression) extends DefBodyGuardExpr
case class BodyGuardsExpresion(guards: List[DefBodyGuardExpr]) extends Expression
case class BodyGuardsExpresionAndWhere(guards: List[DefBodyGuardExpr], whereBlock: WhereBlock) extends Expression





