package parser.ast.module

import lexer._
import parser._

import scala.collection.mutable

case class Module(name: String, imports: Option[List[ImportClause]],decls: List[LangNode]) extends LangNode

object Module  {

  def parse(tokens: TokenStream, nameFromFile: String): Module = {
    val moduleName = if (!tokens.peek(MODULE)) {
      nameFromFile
    } else {
      tokens.consume(MODULE)
      if (tokens.peek(classOf[TID])) tokens.consume(classOf[TID]).value else tokens.consume(classOf[ID]).value
    }
    parseModule(moduleName, tokens)
  }

  private[this] def parseModule(moduleName: String, tokens: TokenStream) : Module = {
    tokens.consumeOptionals(NL)
    Module(moduleName, parseImports(tokens), parseModuleNodes(tokens))
  }


  def parseImports(tokens: TokenStream): Option[List[ImportClause]] = {
    tokens.consumeOptionals(NL)
    var listOfImports = List.empty[ImportClause]
    while (tokens.peek(IMPORT) || tokens.peek(FROM)) {
      if (tokens.peek(IMPORT)) {
        listOfImports = parseImport(tokens) :: listOfImports
      }
      else if (tokens.peek(FROM)) {
        listOfImports = parseFromImport(tokens) :: listOfImports
      }
      tokens.consumeOptionals(NL)
    }
    if (listOfImports.isEmpty) {
      None
    }
    else {
      Some(listOfImports.reverse)
    }
  }

  def parseImport(tokens:TokenStream): ImportClause = {
    tokens.consume(IMPORT)
    if (parseTag(tokens) == ":jvm") {
      JvmImport(parseListOfAlias(tokens))
    }
    else {
      CljImport(parseListOfAlias(tokens))
    }
  }

  def parseFromImport(tokens:TokenStream): ImportClause = {
    tokens.consume(FROM)
    val tag = parseTag(tokens)
    val name = tokens.consume(classOf[ID]).value
    tokens.consume(IMPORT)
    if (tag == ":jvm") {
      FromJvmRequire(name, parseListOfAlias(tokens))
    }
    else {
      FromCljRequire(name, parseListOfAlias(tokens))
    }
  }

  def parseTag(tokens:TokenStream): String = {
    if (tokens.peek(LBRACKET)) {
      tokens.consume(LBRACKET)
      val tag = tokens.consume(classOf[ATOM]).value
      tokens.consume(RBRACKET)
      tag
    } else {
      ""
    }
  }

  def parseListOfAlias(tokens:TokenStream) : List[ImportAlias] = {
    var listOfAlias = List.empty[ImportAlias]
    val impAlias = parseImportAlias(tokens)
    listOfAlias = impAlias :: listOfAlias
    while (tokens.peek(COMMA)) {
      tokens.consume(COMMA)
      tokens.consumeOptionals(NL)
      val impAlias = parseImportAlias(tokens)
      listOfAlias = impAlias :: listOfAlias
    }
    listOfAlias.reverse
  }

  def parseImportAlias(tokens:TokenStream): ImportAlias = {
    val id = if (tokens.peek(classOf[TID])) tokens.consume(classOf[TID]).value else tokens.consume(classOf[ID]).value
    val alias = if (!tokens.peek(AS)) {
      None
    } else {
      tokens.consume(AS)
      Some(tokens.consume(classOf[ID]).value)
    }
    ImportAlias(id, alias)
  }



  def parseModuleNodes(tokens:TokenStream): List[LangNode] = {
    println(s"@@@ parse module nodes (tokens=$tokens)")
    var result = List.empty[LangNode]
    while (tokens.nonEmpty) {
      var inner = false
      if (tokens.peek(PRIVATE)) {
        tokens.consume(PRIVATE)
        inner = true
      }
      if (tokens.peek(CLASS)) {
        result = parseClass(inner, tokens) :: result
      }
      else if (tokens.peek(DATA)) {
        result = parseData(inner, tokens) :: result
      }
      else if (tokens.peek(DEF)) {
        result = multiDef(parseDef(inner, tokens)) :: result
      }
      else if (tokens.peek(DISPATCH)) {
        result = parseDispatch(inner, tokens) :: result
      }
      else if (tokens.peek(EXTENDS)) {
        result = parseExtends(inner, tokens) :: result
      }
      else if (tokens.peek(RECORD)) {
        result = parseRecord(inner, tokens) :: result
      }
      else if (tokens.peek(TRAIT)) {
        result = parseTrait(inner, tokens) :: result
      }
      else {
        result = TopLevelExpression(parsePipedExpr(tokens)) :: result
      }
      tokens.consumeOptionals(NL)
      //println(s"PARSED SO FAR: ${result.reverse}\n\n")
    }
    filter(result.reverse)
  }

  def parseRecord(inner: Boolean, tokens:TokenStream): LangNode = {
    tokens.consume(RECORD)
    val name = tokens.consume(classOf[TID]).value
    tokens.consume(LCURLY)
    var args = parseListOfIds(tokens)
    tokens.consume(RCURLY)
    RecordDecl(name, args)
  }

  def parseListOfIds(tokens:TokenStream): List[String] = {
    val arg = tokens.consume(classOf[ID]).value
    var args = List(arg)
    while (tokens.peek(COMMA)) {
      tokens.consume(COMMA)
      val arg = tokens.consume(classOf[ID]).value
      args = arg :: args
    }
    args.reverse
  }

  def parseExtends(inner: Boolean, tokens:TokenStream): LangNode = {
    tokens.consume(EXTENDS)
    val cls = tokens.consume(classOf[TID]).value
    tokens.consume(WITH)
    val trt = tokens.consume(classOf[TID]).value
    tokens.consumeOptionals(NL)
    val defs = if (!tokens.peek(INDENT)) None else {
      tokens.consume(INDENT)
      var defs = List.empty[ClassMethodDecl]
      while (tokens.peek(DEF)) {
        val defDecl = parseDef(false, tokens)
        defs = ClassMethodDecl(defDecl) :: defs
        tokens.consumeOptionals(NL)
      }
      tokens.consume(DEDENT)
      Some(defs.reverse)
    }
    ExtendsDecl(cls, trt, defs)
  }

  def parseClass(inner: Boolean, tokens:TokenStream): LangNode = {
    tokens.consume(CLASS)
    val name = tokens.consume(classOf[TID]).value
    val argsOpt = if (!tokens.peek(LPAREN)) None else {
      tokens.consume(LPAREN)
      val args = parseListOfIds(tokens)
      tokens.consume(RPAREN)
      Some(args)
    }
    tokens.consume(NL)
    var traits = List.empty[TraitDef]
    if (tokens.peek(INDENT)) {
      tokens.consume(INDENT)
      while (!tokens.peek(DEDENT)) {
        tokens.consume(EXTENDS)
        val traitName = tokens.consume(classOf[TID]).value
        tokens.consumeOptionals(NL)
        tokens.consume(INDENT)
        var traitMethods = List.empty[ClassMethodDecl]
        while (tokens.peek(DEF)) {
          val defDecl = parseDef(false, tokens)
          traitMethods = ClassMethodDecl(defDecl) :: traitMethods
          tokens.consumeOptionals(NL)
        }
        traits = TraitDef(traitName, traitMethods.reverse) :: traits
        tokens.consume(DEDENT)
      }
      tokens.consume(DEDENT)
    }
    ClassDecl(inner, name, argsOpt, traits)
  }

  def parseDispatch(inner: Boolean, tokens:TokenStream): LangNode = {
    tokens.consume(DISPATCH)
    val id = tokens.consume(classOf[ID]).value
    tokens.consume(WITH)
    if (tokens.peek(CLASS)) {
      tokens.consume(CLASS)
      DispatchDecl(id, ClassDispatcher)
    } else {
      val expr = parsePipedExpr(tokens)
      DispatchDecl(id, ExpressionDispatcher(expr))
    }
  }

  def parseData(inner: Boolean, tokens:TokenStream): LangNode = {
    tokens.consume(DATA)
    val id = tokens.consume(classOf[TID]).value
    tokens.consume(ASSIGN)
    var indents = 0
    if (tokens.peek(NL)) {
      tokens.consumeOptionals(NL)
      tokens.consume(INDENT)
      indents += 1
    }
    var adts = List.empty[ADT]
    val adt = parseADT(tokens)
    adts = adt :: adts
    while (tokens.peek(GUARD)) {
      tokens.consume(GUARD)
      tokens.consumeOptionals(NL)
      if (tokens.peek(INDENT)) {
        tokens.consume(INDENT)
        indents += 1
      }
      val adt = parseADT(tokens)
      adts = adt :: adts
    }
    while (indents > 0) {
      tokens.consume(DEDENT)
      indents -= 1
    }
    AdtDecl(id, adts.reverse)
  }

  def parseADT(tokens:TokenStream): ADT = {
    val id = tokens.consume(classOf[TID]).value
    var args = List.empty[String]
    if (tokens.peek(LPAREN)) {
      tokens.consume(LPAREN)
      val arg = tokens.consume(classOf[ID]).value
      args = arg :: args
      while (tokens.peek(COMMA)) {
        tokens.consume(COMMA)
        tokens.consumeOptionals(NL)
        val arg = tokens.consume(classOf[ID]).value
        args = arg :: args
      }
      tokens.consume(RPAREN)
    }
    ADT(id, args.reverse)
  }

  def parseTrait(inner: Boolean, tokens:TokenStream): LangNode = {
    tokens.consume(TRAIT)
    val id = tokens.consume(classOf[TID]).value
    tokens.consumeOptionals(NL)
    var decls = List.empty[TraitMethodDecl]
    if (tokens.peek(INDENT)) {
      tokens.consume(INDENT)
      while (tokens.peek(DEF)) {
        val decl = parseTraitMethodDecl(tokens)
        tokens.consumeOptionals(NL)
        decls = decl :: decls
      }
      tokens.consume(DEDENT)
    }
    TraitDecl(inner, id, decls)
  }

  def parseTraitMethodDecl(tokens:TokenStream): TraitMethodDecl = {
    tokens.consume(DEF)
    val id = tokens.consume(classOf[ID]).value
    var args = List.empty[String]
    while (tokens.peek(classOf[ID])) {
      val arg = tokens.consume(classOf[ID]).value
      args = arg :: args
    }
    tokens.consume(NL)
    TraitMethodDecl(id, args.reverse)
  }

  val defs = mutable.HashMap.empty[String, MultiDefDecl]

  private[this] def multiDef(node: LangNode): LangNode = {
    node match {
      case decl: SimpleDefDecl =>
        if (defs.contains(decl.id)) {
          defs.get(decl.id).map { defDecl =>
            val decls = decl :: defDecl.decls
            val mDecl = MultiDefDecl(defDecl.id, decls)
            defs.update(mDecl.id, mDecl)
            mDecl
          }.get
        }
        else {
          val mDecl = MultiDefDecl(decl.id, List(decl))
          defs.put(mDecl.id, mDecl)
          mDecl
        }
      case d => d
    }
  }

  private[this] def filter(nodes: List[LangNode]): List[LangNode] = {
    var result = List.empty[LangNode]
    for (node <- nodes) {
      node match {
        case md: MultiDefDecl =>
          defs.get(md.id).map { md =>
            val multiDef = MultiDefDecl(md.id, md.decls.reverse)
            if (multiDef.decls.length == 1)
              result = multiDef.decls.head :: result
            else
              result = multiDef :: result
            defs.remove(md.id)
          }
        case _ =>
          result = node :: result
      }
    }
    result.reverse
  }

  def parseDef(inner: Boolean, tokens:TokenStream): DefDecl = {
    tokens.consume(DEF)
    val defId = tokens.consume(classOf[ID]).value
    val (matches, args) = parseDefArgs(tokens)
    if (tokens.peek(NL)) {
      tokens.consume(NL)
      val body = parseDefBodyGuards(tokens)
      body match {
        case bd: BodyGuardsExpresionAndWhere =>
          if (matches.isEmpty) {
            SimpleDefDecl(inner, defId, args, BodyGuardsExpresion(bd.guards), Some(bd.whereBlock))
          }
          else {
            MultiMethod(inner, defId, matches.get, args, BodyGuardsExpresion(bd.guards), Some(bd.whereBlock))
          }
        case _ =>
          val where = tryParseWhereBlock(tokens)
          if (matches.isEmpty) {
            SimpleDefDecl(inner, defId, args, body, where)
          }
          else {
            MultiMethod(inner, defId, matches.get, args, body, where)
          }
      }
    }
    else if (tokens.peek(ASSIGN)) {
      tokens.consume(ASSIGN)
      val body = if (!tokens.peek(NL)) {
        parsePipedExpr(tokens)
      }
      else {
        tokens.consume(NL)
        parseBlockExpr(tokens)
      }
      val where = tryParseWhereBlock(tokens)
      if (matches.isEmpty) {
        SimpleDefDecl(inner, defId, args, body, where)
      } else {
        MultiMethod(inner, defId, matches.get, args, body, where)
      }
    } else {
      throw InvalidDef()
    }
  }

  def parseDefBodyGuards(tokens:TokenStream): Expression = {
    tokens.consume(INDENT)
    var listOfGuards = List.empty[DefBodyGuardExpr]
    var guard = parseBodyGuard(tokens)
    listOfGuards = guard :: listOfGuards
    while (tokens.peek(GUARD)) {
      guard = parseBodyGuard(tokens)
      listOfGuards = guard :: listOfGuards
    }
    val result = if (tokens.peek(WHERE)) {
      BodyGuardsExpresionAndWhere(listOfGuards.reverse, parseUnindentedWhereBlock(tokens))
    } else {
      BodyGuardsExpresion(listOfGuards.reverse)
    }
    tokens.consume(DEDENT)
    result
  }

  def parseBodyGuard(tokens:TokenStream): DefBodyGuardExpr = {
    tokens.consume(GUARD)
    if (tokens.peek(OTHERWISE)) {
      tokens.consume(OTHERWISE)
      tokens.consume(ASSIGN)
      var expr: Expression = null
      if (!tokens.peek(NL)) {
        expr = parsePipedExpr(tokens)
        tokens.consumeOptionals(NL)
      }
      else {
        tokens.consume(NL)
        expr = parseBlockExpr(tokens)
      }
      DefBodyGuardOtherwiseExpression(expr)

    } else {
      val guardExpr = parseLogicalExpr(tokens)
      tokens.consume(ASSIGN)
      var expr: Expression = null
      if (!tokens.peek(NL)) {
        expr = parsePipedExpr(tokens)
        tokens.consume(NL)
      }
      else {
        tokens.consume(NL)
        expr = parseBlockExpr(tokens)
      }
      DefBodyGuardExpression(guardExpr, expr)
    }
  }

  def tryParseWhereBlock(tokens:TokenStream): Option[WhereBlock] = {
    if (tokens.peek(NL) && tokens.peek(2, INDENT)) {
      tokens.consume(NL)
      Some(parseWhereBlock(tokens))
    }
    else if (tokens.peek(WHERE)) {
      Some(parseUnindentedWhereBlock(tokens))
    }
    else {
      None
    }
  }

  def parseWhereBlock(tokens:TokenStream): WhereBlock = {
    tokens.consume(INDENT)
    val whereBlock = parseUnindentedWhereBlock(tokens)
    tokens.consume(DEDENT)
    whereBlock
  }

  def parseUnindentedWhereBlock(tokens:TokenStream): WhereBlock = {
    tokens.consume(WHERE)
    var listOfWhereDefs = List.empty[WhereDef]
    if (!tokens.peek(NL)) {
      val whereDef = parseWhereDef(tokens)
      listOfWhereDefs = whereDef :: listOfWhereDefs
      tokens.consumeOptionals(NL)
    } else {
      tokens.consume(NL)
    }
    if (tokens.peek(INDENT)) {
      tokens.consume(INDENT)
      while (!tokens.peek(DEDENT)) {
        val whereDef = parseWhereDef(tokens)
        listOfWhereDefs = whereDef :: listOfWhereDefs
        tokens.consumeOptionals(NL)
      }
      tokens.consume(DEDENT)
    }
    WhereBlock(listOfWhereDefs.reverse)
  }

  def parseWhereDef(tokens:TokenStream): WhereDef = {
    val listOfIds = if (!tokens.peek(LPAREN)) {
      List(tokens.consume(classOf[ID]).value)
    } else {
      tokens.consume(LPAREN)
      val l = parseListOfIds(tokens)
      tokens.consume(RPAREN)
      l
    }
    var listOfArgs = List.empty[Expression]
    while (!tokens.peek(ASSIGN) && !tokens.peek(GUARD) && !tokens.peek(NL)) {
      val expr = parseWhereArg(tokens)
      listOfArgs = expr :: listOfArgs
    }
    if (tokens.peek(ASSIGN)) {
      tokens.consume(ASSIGN)
      val body = if (!tokens.peek(NL)) {
        parsePipedExpr(tokens)
      } else {
        tokens.consume(NL)
        parseBlockExpr(tokens)
      }
      if (listOfIds.size == 1)
        WhereDefSimple(listOfIds.head, if (listOfArgs.isEmpty) None else Some(listOfArgs.reverse), body)
      else
        WhereDefTupled(listOfIds, if (listOfArgs.isEmpty) None else Some(listOfArgs.reverse), body)
    } else if (tokens.peek(GUARD) || tokens.peek(NL)) {
      var inIndent = false
      tokens.consumeOptionals(NL)
      if (tokens.peek(INDENT)) {
        inIndent = true
        tokens.consume(INDENT)
      }
      var guards = List.empty[WhereGuard]
      while (tokens.peek(GUARD) || tokens.peek(INDENT)) {
        if (!tokens.peek(INDENT)) {
          guards = parseWhereGuard(tokens) :: guards
        }
        else {
          tokens.consume(INDENT)
          while (tokens.peek(GUARD)) {
            guards = parseWhereGuard(tokens) :: guards
          }
          tokens.consume(DEDENT)
        }
      }

      if (inIndent) {
        tokens.consume(DEDENT)
      }
      if (listOfIds.size == 1)
        WhereDefWithGuards(listOfIds.head, if (listOfArgs.isEmpty) None else Some(listOfArgs), guards.reverse)
      else
        WhereDefTupledWithGuards(listOfIds, if (listOfArgs.isEmpty) None else Some(listOfArgs), guards.reverse)
    }
    else {
      throw InvalidDef()
    }
  }

  def parseWhereGuard(tokens:TokenStream): WhereGuard = {
    tokens.consume(GUARD)
    val comp = if (tokens.peek(OTHERWISE)) {
      tokens.consume(OTHERWISE)
      None
    } else {
      Some(parseLogicalExpr(tokens))
    }
    tokens.consume(ASSIGN)
    val body = if (tokens.peek(INDENT)) parseBlockExpr(tokens) else parsePipedExpr(tokens)
    tokens.consume(NL)
    WhereGuard(comp, body)
  }

  def parseWhereArg(tokens: TokenStream) : Expression = {
    if (tokens.peek(classOf[ID])) {
      val id = tokens.consume(classOf[ID])
      Identifier(id.value)
    } else {
      parseLogicalExpr(tokens)
    }
  }

  def parseDefArgs(tokens:TokenStream) : (Option[List[DefArg]], List[DefArg]) = {
    var result = List.empty[DefArg]
    var beforeQuestion = List.empty[DefArg]
    while (!tokens.peek(ASSIGN) && !tokens.peek(NL)) {
      if (tokens.peek(QUESTION)) {
        tokens.consume(QUESTION)
        beforeQuestion = result ++ beforeQuestion
        result = List.empty[DefArg]
      }
      if (tokens.peek(OTHERWISE)) {
        tokens.consume(OTHERWISE)
        result = DefOtherwiseArg :: result
      } else {
        val expr = parseDefArg(tokens)
        result = DefArg(expr) :: result
      }

    }
    (if (beforeQuestion.isEmpty) None else Some(beforeQuestion.reverse), result.reverse)
  }

  def parseDefArg(tokens:TokenStream) : Expression = {
    if (tokens.peek(classOf[ID])) {
      val id = tokens.consume(classOf[ID])
      if (!tokens.peek(COLON)) {
        Identifier(id.value)
      } else {
        tokens.consume(COLON)
        IdIsType(id.value, tokens.consume(classOf[TID]).value)
      }
    }
    else {
      parseLogicalExpr(tokens)
    }
  }

  def parseLetExpr(tokens:TokenStream) : Expression = {
    LetDeclExpr(parseListOfLetVars(tokens, LET), parseInBodyOptExpr(tokens))
  }

  def parseVarExpr(tokens:TokenStream): Expression = {
    VarDeclExpr(parseListOfLetVars(tokens, VAR), parseInBodyOptExpr(tokens))
  }

  def parseBindExpr(tokens:TokenStream): Expression = {
    val listOfLetVars = parseListOfLetVars(tokens, BIND)
    parseInBodyOptExpr(tokens) match {
      case None => throw InvalidExpression()
      case Some(body) =>  BindDeclExpr(listOfLetVars.reverse, body)
    }
  }

  def parseListOfLetVars(tokens:TokenStream, token: TOKEN) : List[Variable] = {
    tokens.consume(token)
    tokens.consumeOptionals(NL)
    var insideIndent = if (tokens.peek(INDENT)) 1 else 0
    if (insideIndent == 1)
      tokens.consume(INDENT)
    var letVar = parseLetVar(tokens)
    var listOfLetVars = List.empty[Variable]
    listOfLetVars = letVar :: listOfLetVars
    while (tokens.peek(COMMA)) {
      tokens.consume(COMMA)
      tokens.consumeOptionals(NL)
      if (tokens.peek(INDENT)) {
        tokens.consume(INDENT)
        insideIndent += 1
      }
      letVar = parseLetVar(tokens)
      listOfLetVars = letVar :: listOfLetVars
    }

    while (insideIndent > 0) {
      tokens.consumeOptionals(NL)
      tokens.consume(DEDENT)
      insideIndent -= 1
    }
    listOfLetVars.reverse
  }

  def parseInBodyExpr(tokens:TokenStream): Option[Expression] = {
    tokens.consume(IN)
    if (!tokens.peek(NL)) {
      Some(parsePipedExpr(tokens))
    } else {
      tokens.consume(NL)
      Some(parseBlockExpr(tokens))
    }
  }

  def parseInBodyOptExpr(tokens:TokenStream) : Option[Expression] = {
    if (tokens.peek(IN)) {
      parseInBodyExpr(tokens)
    } else if (tokens.peek(NL) && tokens.peek(2, IN)) {
      tokens.consume(NL)
      parseInBodyExpr(tokens)
    } else if (tokens.peek(NL) && tokens.peek(2, INDENT) && tokens.peek(3, IN)) {
      tokens.consume(NL)
      tokens.consume(INDENT)
      val result = parseInBodyExpr(tokens)
      tokens.consume(DEDENT)
      result
    } else {
      None
    }
  }

  def parseLetVar(tokens:TokenStream) : Variable = {
    tokens.consumeOptionals(NL)
    val id = parseLetId(tokens)
    tokens.consume(ASSIGN)
    val expr = parsePipedOrBodyExpression(tokens)
    LetVariable(id, expr)
  }

  def parseLetId(tokens:TokenStream) : LetId = {
    if (!tokens.peek(LPAREN)) {
      val idToken = tokens.consume(classOf[ID])
      LetSimpleId(idToken.value)
    } else {
      tokens.consume(LPAREN)
      var ids = List.empty[LetId]
      val id = parseLetId(tokens)
      ids =id :: ids
      while (tokens.peek(COMMA)) {
        tokens.consume(COMMA)
        val id = parseLetId(tokens)
        ids = id :: ids
      }
      tokens.consume(RPAREN)
      LetTupledId(ids)
    }
  }

  private def parsePipedOrBodyExpression(tokens:TokenStream): Expression = {
    if (!tokens.peek(NL))
      parsePipedExpr(tokens)
    else {
      tokens.consume(NL)
      parseBlockExpr(tokens)
    }
  }

  // pipedExpr = expr (PIPE_OPER pipedExpr)*
  def parsePipedExpr(tokens:TokenStream) : Expression = {
    val expr = parseForwardPipeExpr(tokens:TokenStream)
    expr
  }

  def parseForwardPipeExpr(tokens:TokenStream) : Expression = {
    var expr = parseForwardPipeFirstArgExpr(tokens)
    if (tokens.peek(PIPE_RIGHT)) {
      var value = expr
      var args = List.empty[Expression]
      while (tokens.peek(PIPE_RIGHT)) {
        args = value :: args
        tokens.consume(PIPE_RIGHT)
        value = parseForwardPipeFirstArgExpr(tokens)
      }
      args = value :: args
      expr = ForwardPipeFuncCallExpression(args.reverse)
    }
    expr
  }

  def parseForwardPipeFirstArgExpr(tokens:TokenStream) : Expression = {
    var expr = parseBackwardPipeExpr(tokens)
    if (tokens.peek(PIPE_RIGHT_FIRST_ARG)) {
      var value = expr
      var args = List.empty[Expression]
      while (tokens.peek(PIPE_RIGHT_FIRST_ARG)) {
        args = value :: args
        tokens.consume(PIPE_RIGHT_FIRST_ARG)
        value = parseDollarExpr(tokens)
      }
      args = value :: args
      expr = ForwardPipeFirstArgFuncCallExpression(args.reverse)
    }
    expr
  }

  def parseBackwardPipeExpr(tokens:TokenStream) : Expression = {
    var expr = parseBackwardFirstArgPipeExpr(tokens)
    if (tokens.peek(PIPE_LEFT)) {
      var value = expr
      var args = List.empty[Expression]
      while (tokens.peek(PIPE_LEFT)) {
        args = value :: args
        tokens.consume(PIPE_LEFT)
        value = parseBackwardFirstArgPipeExpr(tokens)
      }
      args = value :: args
      expr = BackwardPipeFuncCallExpression(args)
    }
    expr
  }

  def parseBackwardFirstArgPipeExpr(tokens:TokenStream) : Expression = {
    var expr = parseDollarExpr(tokens)
    if (tokens.peek(PIPE_LEFT_FIRST_ARG)) {
      var value = expr
      var args = List.empty[Expression]
      while (tokens.peek(PIPE_LEFT_FIRST_ARG)) {
        args = value :: args
        tokens.consume(PIPE_LEFT_FIRST_ARG)
        value = parseDollarExpr(tokens)
      }
      args = value :: args
      expr = BackwardPipeFirstArgFuncCallExpression(args)
    }
    expr
  }

  def parseDollarExpr(tokens:TokenStream) : Expression = {
    var expr = parseExpr(tokens)
    if (tokens.peek(DOLLAR)) {
      val func = expr
      tokens.consume(DOLLAR)
      var args = List.empty[Expression]
      while (!funcCallEndToken(tokens)) {
        expr = parseDollarExpr(tokens)
        args = expr :: args
      }
      expr = FunctionCallWithDollarExpression(func, args.reverse)
    }
    expr
  }


  // funcCallExpr ::= control_expr | lambda_expr
  def parseExpr(tokens:TokenStream) : Expression = {
    // if control
    if (tokens.peek(classOf[CONTROL]))
      parseControlExpr(tokens)
    else if (tokens.peek(LET))
      parseLetExpr(tokens)
    else if (tokens.peek(VAR))
      parseVarExpr(tokens)
    else if (tokens.peek(BIND))
      parseBindExpr(tokens)
    else
      parseLambdaExpr(tokens)
  }

  def parseControlExpr(tokens:TokenStream) : Expression = {
    if (tokens.peek(COND)) {
      parseCondExpr(tokens)
    }
    else if (tokens.peek(FOR)) {
      parseForExpr(tokens)
    }
    else if (tokens.peek(IF)) {
      parseIfExpr(tokens)
    }
    else if (tokens.peek(WHEN)) {
      parseWhenExpr(tokens)
    }
    else if (tokens.peek(LOOP)) {
      parseLoopExpr(tokens)
    }
    else if (tokens.peek(UNTIL)) {
      parseWhileExpr(tokens)
    }
    else if (tokens.peek(WHILE)) {
      parseWhileExpr(tokens)
    }
    else if (tokens.peek(RECUR)) {
      parseRecurExpr(tokens)
    }
    else if (tokens.peek(REIFY)) {
      parseReifyExpr(tokens)
    }
    else if (tokens.peek(REPEAT)) {
      parseRepeatExpr(tokens)
    }
    else if (tokens.peek(SET)) {
      parseAssignExpr(tokens)
    }
    else if (tokens.peek(TRY)) {
      parseTryExpr(tokens)
    }
    else if (tokens.peek(THROW)) {
      parseThrowExpr(tokens)
    }
    else {
      println(s"ERROR PARSE CONTROL tokens= $tokens")
      throw InvalidNodeException(tokens.nextToken())
    }
  }

  def parseReifyExpr(tokens:TokenStream) : Expression = {
    tokens.consume(REIFY)
    val name = tokens.consume(classOf[TID]).value
    tokens.consume(NL)
    tokens.consume(INDENT)
    var methods = List.empty[ClassMethodDecl]
    while (tokens.peek(DEF)) {
      val defDecl = parseDef(false, tokens)
      methods = ClassMethodDecl(defDecl) :: methods
      tokens.consumeOptionals(NL)
    }
    tokens.consume(DEDENT)
    ReifyExpression(name, methods.reverse)
  }

  def parseThrowExpr(tokens:TokenStream) : Expression = {
    tokens.consume(THROW)
    val ctor = parseConstructorExpr(tokens)
    tokens.consumeOptionals(NL)
    ThrowExpression(ctor)
  }

  def parseTryExpr(tokens:TokenStream) : Expression = {
    tokens.consume(TRY)
    val body = parsePipedOrBodyExpression(tokens)
    tokens.consumeOptionals(NL)
    var catches = List.empty[CatchExpression]
    var indents = 0
    if (tokens.peek(INDENT)) {
      tokens.consume(INDENT)
      indents += 1
    }
    while (tokens.peek(CATCH)) {
      val catchExpr = parseCatchExpr(tokens)
      catches = catchExpr :: catches
      tokens.consumeOptionals(NL)
      if (tokens.peek(INDENT)) {
        tokens.consume(INDENT)
        indents += 1
      }
    }
    val finallyExpr = if (tokens.peek(FINALLY)) {
      Some(parseFinallyExpr(tokens))
    } else {
      None
    }
    while (indents > 0) {
      tokens.consume(DEDENT)
      indents -= 1
    }
    TryExpression(body, catches.reverse, finallyExpr)
  }

  def parseCatchExpr(tokens:TokenStream): CatchExpression = {
    tokens.consume(CATCH)
    if (tokens.peek(classOf[ID])) {
      val id = tokens.consume(classOf[ID]).value
      tokens.consume(COLON)
      val ex = tokens.consume(classOf[TID]).value
      tokens.consume(ARROW)
      CatchExpression(Some(id), ex, parsePipedOrBodyExpression(tokens))
    } else {
      val ex = tokens.consume(classOf[TID]).value
      tokens.consume(ARROW)
      CatchExpression(None, ex, parsePipedOrBodyExpression(tokens))
    }
  }

  def parseFinallyExpr(tokens:TokenStream) : Expression = {
    tokens.consume(FINALLY)
    tokens.consume(ARROW)
    parsePipedOrBodyExpression(tokens)
  }

  def parseCondExpr(tokens:TokenStream): Expression = {
    tokens.consume(COND)
    tokens.consume(NL)
    tokens.consume(INDENT)
    var guards = List.empty[CondGuard]
    while (!tokens.peek(DEDENT)) {
      val comp = if (tokens.peek(OTHERWISE)) {
        tokens.consume(OTHERWISE)
        None
      } else {
        Some(parseLogicalExpr(tokens))
      }
      tokens.consume(ARROW)
      val value = parsePipedExpr(tokens)
      tokens.consumeOptionals(NL)
      guards = CondGuard(comp, value) :: guards
    }
    tokens.consume(DEDENT)
    CondExpression(guards.reverse)
  }

  def parseRepeatExpr(tokens:TokenStream): Expression = {
    tokens.consume(REPEAT)
    if (tokens.peek(WITH))
      tokens.consume(WITH)
    var repeatVariables = List.empty[RepeatNewVarValue]
    var repVar = parseRepeatNewValue(tokens)
    repeatVariables = repVar :: repeatVariables
    while (tokens.peek(COMMA)) {
      tokens.consume(COMMA)
      repVar = parseRepeatNewValue(tokens)
      repeatVariables = repVar :: repeatVariables
    }
    tokens.consumeOptionals(NL)
    RepeatExpr(Some(repeatVariables.reverse))
  }

  def parseRepeatNewValue(tokens:TokenStream) : RepeatNewVarValue = {
    if (tokens.peek(classOf[ID]) && tokens.peek(2, ASSIGN)) {
      val id = tokens.consume(classOf[ID])
      tokens.consume(ASSIGN)
      val expr = parsePipedExpr(tokens)
      RepeatNewVarValue(id.value, expr)
    } else {
      RepeatNewVarValue(genId(), parseExpr(tokens))
    }
  }

  def genId() : String = {
    s"id_${java.util.UUID.randomUUID.toString}"
  }

  def parseForExpr(tokens:TokenStream) : Expression = {
    tokens.consume(FOR)
    val forDecls = parseForDecls(tokens)
    val forBody = parseForBody(tokens)
    ForExpression(forDecls, forBody)
  }

  def parseForDecls(tokens:TokenStream) : List[LoopDeclVariable] = {
    var listOfDecls = List.empty[LoopDeclVariable]
    val forVarDecl = parseForVarDecl(tokens)
    listOfDecls = forVarDecl :: listOfDecls

    while (tokens.peek(COMMA)) {
      tokens.consume(COMMA)
      val forVarDecl = parseForVarDecl(tokens)
      listOfDecls = forVarDecl :: listOfDecls
    }
    listOfDecls.reverse
  }

  def parseForVarDecl(tokens:TokenStream) : LoopDeclVariable = {
    if (!tokens.peek(LPAREN)) {
      val id = tokens.consume(classOf[ID])
      tokens.consume(IN)
      ForVarDeclIn(id.value, parsePipedExpr(tokens))
    }
    else {
      tokens.consume(LPAREN)
      var ids = List.empty[String]
      val id = tokens.consume(classOf[ID])
      ids = id.value :: ids
      while (tokens.peek(COMMA)) {
        tokens.consume(COMMA)
        val id = tokens.consume(classOf[ID])
        ids = id.value :: ids
      }
      tokens.consume(RPAREN)
      tokens.consume(IN)
      ForVarDeclTupledIn(ids, parsePipedExpr(tokens))
    }
  }

  def parseWhileExpr(tokens:TokenStream) : Expression = {
    tokens.consume(WHILE)
    val comp = parseLogicalExpr(tokens)
    tokens.consume(DO)
    WhileExpression(comp, parsePipedOrBodyExpression(tokens))
  }


  def parseUntilExpr(tokens:TokenStream) : Expression = {
    tokens.consume(UNTIL)
    val comp = parseLogicalExpr(tokens)
    tokens.consume(DO)
    UntilExpression(comp, parsePipedOrBodyExpression(tokens))
  }

  def parseLoopExpr(tokens:TokenStream) : Expression = {
    tokens.consume(LOOP)

    val loopDecls = parseLoopDecls(tokens)

    var guardExpr : Option[LoopGuard] = None
    tokens.consumeOptionals(NL)

    if (tokens.peek(WHILE)) {
      tokens.consume(WHILE)
      guardExpr = Some(WhileGuardExpr(parseLogicalExpr(tokens)))
    }


    if (tokens.peek(UNTIL)) {
      if (guardExpr.isDefined)
        throw InvalidUntilAlreadyHasWhile()
      tokens.consume(UNTIL)
      guardExpr = Some(UntilGuardExpr(parseLogicalExpr(tokens)))
    }

    tokens.consume(DO)
    if (!tokens.peek(NL)) {
      LoopExpression(loopDecls, guardExpr, parsePipedExpr(tokens))
    }
    else {
      tokens.consume(NL)
      LoopExpression(loopDecls, guardExpr, parseBlockExpr(tokens))
    }
  }

  def parseLoopDecls(tokens:TokenStream) : List[LoopVarDecl] = {
    var listOfDecls = List.empty[LoopVarDecl]
    var loopVarDecl = parseLoopVarDecl(tokens)
    listOfDecls = loopVarDecl :: listOfDecls
    while (tokens.peek(COMMA)) {
      tokens.consume(COMMA)
      loopVarDecl = parseLoopVarDecl(tokens)
      listOfDecls = loopVarDecl :: listOfDecls
    }
    listOfDecls.reverse
  }

  def parseLoopVarDecl(tokens:TokenStream) : LoopVarDecl = {
    val id = tokens.consume(classOf[ID])
    if (tokens.peek(ASSIGN)) {
      tokens.consume(ASSIGN)
      LoopVarDecl(id.value, parsePipedExpr(tokens))
    } else {
      throw UnexpectedTokenClassException()
    }
  }

  def parseForBody(tokens:TokenStream) : Expression = {
    tokens.consume(DO)
    parsePipedOrBodyExpression(tokens)
  }

  def parseBlockExpr(tokens:TokenStream) : Expression = {
    tokens.consume(INDENT)
    var listOfExpressions = List.empty[Expression]
    var loop = 0
    while (!tokens.peek(DEDENT)) {
      loop += 1
      val expr = parsePipedExpr(tokens)
      tokens.consumeOptionals(NL)
      listOfExpressions = expr :: listOfExpressions
    }
    tokens.consume(DEDENT)
    BlockExpression(listOfExpressions.reverse)
  }

  def parseIfExpr(tokens:TokenStream) : Expression = {
    tokens.consume(IF)
    val comp = parseLogicalExpr(tokens)
    tokens.consume(THEN)
    val thenPart = if (!tokens.peek(NL)) {
      parsePipedExpr(tokens)
    }
    else {
      tokens.consume(NL)
      parseBlockExpr(tokens)
    }
    while (tokens.peek(NL)) tokens.consume(NL)
    var elif = List.empty[ElifPart]
    if (tokens.peek(ELIF)) {
      while (tokens.peek(ELIF)) {
        tokens.consume(ELIF)
        val elifComp = parseLogicalExpr(tokens)
        tokens.consume(THEN)
        val elifPart = if (!tokens.peek(NL)) {
          ElifPart(elifComp, parsePipedExpr(tokens))
        }
        else {
          while (tokens.peek(NL)) tokens.consume(NL)
          ElifPart(elifComp, parseBlockExpr(tokens))
        }
        elif = elifPart :: elif
      }
    }
    if (tokens.peek(ELSE)) {
      tokens.consume(ELSE)
      if (!tokens.peek(NL)) {
        return IfExpression(comp, thenPart, elif.reverse, parsePipedExpr(tokens))
      } else {
        while (tokens.peek(NL)) tokens.consume(NL)
        return IfExpression(comp, thenPart, elif.reverse, parseBlockExpr(tokens))
      }
    }
    throw InvalidIfExpression()
  }

  def parseWhenExpr(tokens:TokenStream) : Expression = {
    tokens.consume(WHEN)
    val comp = parseLogicalExpr(tokens)
    tokens.consume(THEN)
    if (!tokens.peek(NL)) {
      WhenExpression(comp, parsePipedExpr(tokens))
    }
    else {
      tokens.consume(NL)
      WhenExpression(comp, parseBlockExpr(tokens))
    }
  }

  def parseAssignExpr(tokens:TokenStream) : Expression = {
    tokens.consume(SET)
    val expr = parsePipedOrBodyExpression(tokens)
    if (!expr.isInstanceOf[AssignableExpression])
      throw CantAssignToExpression()
    tokens.consume(ASSIGN)
    val right = parsePipedExpr(tokens)
    SimpleAssignExpr(expr, right)
  }

  def parseLambdaExpr(tokens:TokenStream) : Expression = {
    if (!tokens.peek(LAMBDA)) {
      parseLogicalExpr(tokens)
    }
    else {
      tokens.consume(LAMBDA)
      var args = List.empty[LambdaArg]
      val arg = parseLambdaArg(tokens)
      args = arg :: args
      while (tokens.peek(COMMA)) {
        tokens.consume(COMMA)
        val arg = parseLambdaArg(tokens)
        args = arg :: args
      }
      if (!tokens.peek(ARROW)) {
        throw InvalidLambdaExpression(tokens.nextToken())
      }
      tokens.consume(ARROW)
      val expr = LambdaExpression(args.reverse, parseExpr(tokens))
      expr
    }
  }

  def parseLambdaArg(tokens:TokenStream) : LambdaArg = {
    if (tokens.peek(classOf[ID])) {
      val id = tokens.consume(classOf[ID])
      LambdaSimpleArg(id.value)
    }
    else if (tokens.peek(LPAREN)) {
      tokens.consume(LPAREN)
      var ids = List.empty[String]
      val id = tokens.consume(classOf[ID]).value
      ids = id :: ids
      while (tokens.peek(COMMA)) {
        tokens.consume(COMMA)
        val id = tokens.consume(classOf[ID]).value
        ids = id :: ids
      }
      tokens.consume(RPAREN)
      LambdaTupleArg(ids.reverse)
    } else {
      throw InvalidLambdaExpression(tokens.nextToken())
    }
  }

  def parseLogicalExpr(tokens:TokenStream) : Expression = {
    var expr = parseComparativeExpr(tokens)
    while (tokens.peek(classOf[LOGICAL_BIN_OPER])) {
      val logicalOper = tokens.consume(classOf[LOGICAL_BIN_OPER])
      while (tokens.peek(NL)) tokens.consume(NL)
      expr = classifyLogicalExpr(logicalOper, expr, parseLogicalExpr(tokens))
    }
    expr
  }

  def classifyLogicalExpr(oper: LOGICAL_BIN_OPER, left: Expression, right: Expression) : LogicalExpression = {
    oper match {
      case AND => LogicalAndExpression(left, right)
      case OR => LogicalOrExpression(left, right)
    }
  }

  def parseComparativeExpr(tokens:TokenStream) : Expression = {
    var expr = parseConsExpr(tokens)
    while (tokens.peek(classOf[COMPARATIVE_BIN_OPER])) {
      val oper = tokens.consume(classOf[COMPARATIVE_BIN_OPER])
      while (tokens.peek(NL)) tokens.consume(NL)
      expr = classifyComparativeExpr(oper, expr, parseConsExpr(tokens))
    }
    expr
  }

  def classifyComparativeExpr(oper: COMPARATIVE_BIN_OPER, left: Expression, right: Expression) : ComparativeExpression = {
    oper match {
      case LT => LessThanExpr(left, right)
      case GT => GreaterThanExpr(left, right)
      case LE => LessOrEqualThanExpr(left, right)
      case GE => GreaterOrEqualThanExpr(left, right)
      case EQUALS => EqualsExpr(left, right)
      case NOT_EQUALS => NotEqualsExpr(left, right)
      case MATCH => ReMatchExpr(left, right)
      case MATCHES => MatchExpr(left, right)
      case NOT_MATCHES => NoMatchExpr(left, right)
      case CONTAINS => ContainsExpr(left, right)
    }
  }

  def parseConsExpr(tokens:TokenStream) : Expression = {
    var expr = parseSumExpr(tokens)
    while (tokens.peek(CONS)) {
      tokens.consume(CONS)
      while (tokens.peek(NL)) tokens.consume(NL)
      expr = ConsExpression(expr, parseConsExpr(tokens))
    }
    expr
  }

  def parseSumExpr(tokens:TokenStream) : Expression = {
    var expr = parseMulExpr(tokens)
    while (tokens.peek(classOf[SUM_OPER])) {
      val oper : SUM_OPER = tokens.consume().get
      while (tokens.peek(NL)) tokens.consume(NL)
      expr = classifySumExpr(oper, expr, parseMulExpr(tokens))
    }
    expr
  }

  def classifySumExpr(oper: SUM_OPER, left: Expression, right: Expression) : SumExpression = {
    oper match {
      case PLUS => AddExpression(left, right)
      case MINUS => SubstractExpression(left, right)
      case PLUS_PLUS => ConcatExpression(left, right)
    }
  }

  def parseMulExpr(tokens:TokenStream) : Expression = {
    var expr = parsePowExpr(tokens)
    while (tokens.peek(classOf[MUL_OPER])) {
      val oper = tokens.consume(classOf[MUL_OPER])
      while (tokens.peek(NL)) tokens.consume(NL)
      expr = classifyMulExpr(oper, expr, parsePowExpr(tokens))
    }
    expr
  }

  def classifyMulExpr(oper: MUL_OPER, left: Expression, right: Expression) : MultExpression = {
    oper match {
      case MULT => MultiplyExpression(left, right)
      case DIV => DivideExpression(left, right)
      case MOD => ModExpression(left, right)
      case MULT_BIG => MultiplyBigExpression(left, right)
    }
  }


  def parsePowExpr(tokens:TokenStream) : Expression = {
    var expr = parseComposeExpr(tokens)
    while (tokens.peek(POW)) {
      tokens.consume(POW)
      expr = PowerExpression(expr, parsePowExpr(tokens))
    }
    expr
  }

  def parseComposeExpr(tokens:TokenStream) : Expression = {
    var expr = parsePostfixExpr(tokens)
    while (tokens.peek(classOf[COMPOSE_OPER])) {
      val op = tokens.consume(classOf[COMPOSE_OPER])
      expr = op match {
        case COMPOSE_FORWARD => ComposeExpressionForward(expr, parseComposeExpr(tokens))
        case COMPOSE_BACKWARD => ComposeExpressionBackward(expr, parseComposeExpr(tokens))
      }
    }
    expr
  }

  def parsePostfixExpr(tokens:TokenStream) : Expression = {
    var expr = parsePrimExpr(tokens)
    if (tokens.peek(ARROBA)) {
      val array = expr
      tokens.consume(ARROBA)
      val arg = parseLogicalExpr(tokens)
      expr = ArrayAccessExpression(array, arg)
    }
    expr
  }

  def parsePrimExpr(tokens:TokenStream) : Expression = {
    if (tokens.peek(LPAREN) && tokens.peek(2, classOf[OPER])) {
      parsePartialOper(tokens)
    }
    else if (tokens.peek(LPAREN) || tokens.peek(LBRACKET) || tokens.peek(LCURLY) || tokens.peek(HASHLCURLY)) {
      parseAtomicExpr(tokens)
    }
    else if (tokens.peek(classOf[LITERAL])) {
      parseAtomicExpr(tokens)
    }
    else if (tokens.peek(LAZY)) {
      tokens.consume(LAZY)
      LazyExpression(parsePipedExpr(tokens))
    }
    else if (tokens.peek(NEW)) {
      parseNewCtorExpression(tokens)
    }
    else if (tokens.peek(classOf[TID])) {
      parseConstructorExpr(tokens)
    }
    else {
      parseFuncCallExpr(tokens)
    }
  }

  def parseConstructorExpr(tokens:TokenStream) : ConstructorExpression = {
    val cls = tokens.consume(classOf[TID]).value
    if (tokens.peek(LPAREN)) {
      tokens.consume(LPAREN)
      val args = if (tokens.peek(RPAREN)) List.empty else parseListOfExpressions(tokens)
      tokens.consume(RPAREN)
      ConstructorExpression(isRecord = false, cls, args)
    }
    else if (tokens.peek(LCURLY)) {
      tokens.consume(LCURLY)
      val args = if (tokens.peek(RCURLY)) List.empty else parseListOfExpressions(tokens)
      tokens.consume(RCURLY)
      ConstructorExpression(isRecord = true, cls, args)
    }
    else {
      ConstructorExpression(isRecord = true, cls, List.empty)
    }
  }

  def parseNewCtorExpression(tokens:TokenStream) : Expression = {
    tokens.consume(NEW)
    val cls = tokens.consume(classOf[TID]).value
    tokens.consume(LPAREN)
    val args = if (tokens.peek(RPAREN)) List.empty[Expression] else parseListOfExpressions(tokens)
    tokens.consume(RPAREN)
    NewCallExpression(cls, args)
  }

  def parseListOfExpressions(tokens:TokenStream) : List[Expression] = {
    val expr = parseExpr(tokens)
    var exprs = List(expr)
    while (tokens.peek(COMMA)) {
      tokens.consume(COMMA)
      tokens.consumeOptionals(NL)
      val expr = parseExpr(tokens)
      exprs = expr :: exprs
    }
    exprs.reverse
  }


  def parseRecurExpr(tokens:TokenStream) : Expression = {
    tokens.consume(RECUR)
    var recurArgs = List.empty[Expression]
    while (!funcCallEndToken(tokens)) {
      val arg = parsePipedExpr(tokens)
      recurArgs = arg :: recurArgs
    }
    RecurExpr(recurArgs.reverse)
  }

  def parseFuncCallExpr(tokens:TokenStream) : Expression = {
    var expr : Expression = null
    //println(s"@@parseFunCallExpr (tokens=${tokens})")
    if (tokens.peek(classOf[ID])) {
      val id = tokens.consume(classOf[ID])
      expr = Identifier(id.value)
    }
    else if (tokens.peek(classOf[ATOM])) {
      expr = parseAtom(tokens)
    }
    if (!funcCallEndToken(tokens)) {
      var args = List.empty[Expression]
      val func = expr
      while (!funcCallEndToken(tokens)) {
        if (tokens.peek(classOf[ID])) {
          val id = tokens.consume(classOf[ID])
          expr = Identifier(id.value)
        } else {
          expr = parseDollarExpr(tokens)
        }
        args = expr :: args
      }
      FunctionCallExpression(func, args.reverse)
    } else {
      if (expr == null) {
        println(s"@@INVALID EXPRESSION TOKENS= $tokens")
        throw InvalidExpression()
      }
      expr
    }
  }

  def funcCallEndToken(tokens:TokenStream) : Boolean = {
    if (tokens.isEmpty) {
      true
    } else {
      tokens.nextToken().exists { next =>
        next == NL || next.isInstanceOf[PIPE_OPER] || next.isInstanceOf[OPER] || next.isInstanceOf[DECL] ||
          next == INDENT || next == DEDENT || next == ASSIGN  ||
          next == DOLLAR || next == COMMA || next == LET || next == VAR || next == DO || next == THEN ||
          next == ELSE || next == RPAREN || next == IN || next == RBRACKET || next == RCURLY || next == WHERE
      }
    }

  }

  def parseAtomicExpr(tokens:TokenStream) : Expression = {
    var expr: Expression = null
    if (tokens.peek(LPAREN)) {
      tokens.consume(LPAREN)
      expr = parsePipedExpr(tokens)
      if (tokens.peek(COMMA)) {
        var tupleElem = expr
        var tupleElements = List.empty[Expression]
        tupleElements = tupleElem :: tupleElements
        while (tokens.peek(COMMA)) {
          tokens.consume(COMMA)
          tupleElem = parsePipedExpr(tokens)
          tupleElements = tupleElem :: tupleElements
        }
        if (tokens.peek(DOTDOTDOT)) {
          tokens.consume(DOTDOTDOT)
          expr = InfiniteTupleExpr(tupleElements.reverse)

        } else {
          expr = TupleExpr(tupleElements.reverse)
        }
      }
      if (expr.isInstanceOf[Identifier]) {
        expr = FunctionCallExpression(expr, List.empty[Expression])
      }
      tokens.consume(RPAREN)
    }
    else if (tokens.peek(LBRACKET)) {
      expr = parseRangeExpr(tokens)
    }
    else if (tokens.peek(LCURLY)) {
      expr = parseDictionaryExpr(tokens)
    }
    else if (tokens.peek(HASHLCURLY)) {
      expr = parseSetExpr(tokens)
    }
    else {
      expr = parseLiteral(tokens)
    }


    if (expr == null) {
      println(s"!!! expr == null, tokens = $tokens")
      throw UnexpectedTokenClassException()
    }
    expr
  }

  def parseAtom(tokens:TokenStream) : Expression = {
    val atom = tokens.consume(classOf[ATOM])
    Atom(atom.value)
  }

  def parseLiteral(tokens:TokenStream) : Expression = {
    if (tokens.peek(TRUE)) {
      tokens.consume(TRUE)
      BoolLiteral(true)
    }
    else if (tokens.peek(FALSE)) {
      tokens.consume(FALSE)
      BoolLiteral(false)
    }
    else if (tokens.peek(classOf[INT_LITERAL])) {
      IntLiteral(tokens.consume(classOf[INT_LITERAL]).value)
    }
    else if (tokens.peek(classOf[LONG_LITERAL])) {
      LongLiteral(tokens.consume(classOf[LONG_LITERAL]).value)
    }
    else if (tokens.peek(classOf[BIGINT_LITERAL])) {
      BigIntLiteral(tokens.consume(classOf[BIGINT_LITERAL]).value)
    }
    else if (tokens.peek(classOf[DOUBLE_LITERAL])) {
      DoubleLiteral(tokens.consume(classOf[DOUBLE_LITERAL]).value)
    }
    else if (tokens.peek(classOf[STRING_LITERAL])) {
      StringLiteral(tokens.consume(classOf[STRING_LITERAL]).value)
    }
    else if (tokens.peek(classOf[ISODATETIME_LITERAL])) {
      DateTimeLiteral(tokens.consume(classOf[ISODATETIME_LITERAL]).value)
    }
    else if (tokens.peek(classOf[CHAR_LITERAL])) {
      CharLiteral(tokens.consume(classOf[CHAR_LITERAL]).chr)
    }
    else if (tokens.peek(classOf[REGEXP_LITERAL])) {
      RegexpLiteral(tokens.consume(classOf[REGEXP_LITERAL]).re)
    }
    else if (tokens.peek(classOf[FSTRING_LITERAL])) {
      FStringLiteral(tokens.consume(classOf[FSTRING_LITERAL]).value)
    }
    else  {
      null
    }
  }

  def parsePartialOper(tokens:TokenStream) : Expression = {
    tokens.consume(LPAREN)
    val parOp = tokens.consume(classOf[OPER])
    var listOfArgs: List[Expression] = List.empty[Expression]
    while (!tokens.peek(RPAREN)) {
      val expr = parseLogicalExpr(tokens)
      listOfArgs = expr :: listOfArgs
    }
    tokens.consume(RPAREN)
    classifyPartialOper(parOp, listOfArgs)
  }

  def classifyPartialOper(parOp: OPER, args: List[Expression]) : Expression = {
    parOp match {
      case PLUS => PartialAdd(args)
      case MINUS => PartialSub(args)
      case MULT => PartialMul(args)
      case DIV => PartialDiv(args)
      case MOD => PartialMod(args)
      case EQUALS => PartialEQ(args)
      case NOT_EQUALS => PartialNE(args)
      case LT => PartialLT(args)
      case GT => PartialGT(args)
      case LE => PartialLE(args)
      case GE => PartialGE(args)
      case POW => PartialPow(args)
      case CONS => PartialCons(args)
      case PLUS_PLUS => PartialConcat(args)
      case _ => throw PartialOperNotSupported(parOp)
    }
  }

  def parseRangeExpr(tokens:TokenStream) : Expression = {
    tokens.consume(LBRACKET)
    if (tokens.peek(RBRACKET)) {
      tokens.consume(RBRACKET)
      return EmptyListExpresion()
    }
    var expr = parseLogicalExpr(tokens)
    if (tokens.peek(COMMA)) {
      var list = List.empty[Expression]
      while(tokens.peek(COMMA)) {
        tokens.consume(COMMA)
        list = expr :: list
        expr = parseLogicalExpr(tokens)
      }
      list = expr :: list
      expr = ListExpression(list.reverse, None)
    }
    if (tokens.peek(DOTDOT)) {
      tokens.consume(DOTDOT)
      expr = parseEndRange(tokens, expr, include=true)
    }
    if (tokens.peek(DOTDOTLESS)) {
      tokens.consume(DOTDOTLESS)
      expr = parseEndRange(tokens, expr, include = false)
    }
    if (tokens.peek(DOTDOTDOT)) {
      tokens.consume(DOTDOTDOT)
      expr = InfiniteRangeExpression(expr)
    }
    else if (tokens.peek(GUARD)) {
      tokens.consume(GUARD)
      var listOfGuards = List.empty[ListGuard]
      var guard = parseListGuard(tokens)
      listOfGuards = guard :: listOfGuards
      while (tokens.peek(COMMA)) {
        tokens.consume(COMMA)
        guard = parseListGuard(tokens)
        listOfGuards = guard :: listOfGuards
      }
      expr = ListExpression(List(expr), Some(listOfGuards.reverse))
    }
    tokens.consume(RBRACKET)
    if (!expr.isInstanceOf[ValidRangeExpression])
      expr = ListExpression(List(expr), None)
    expr
  }

  private[this] def parseEndRange(tokens: TokenStream, expr: Expression, include: Boolean) : Expression = {
    expr match {
      case expression: ListExpression if expression.expressions.size == 2 =>
        val lexpr = expression.expressions
        val rangeInit = lexpr.head
        val rangeIncrement = SubstractExpression(lexpr.tail.head, lexpr.head)
        if (include)
          RangeWithIncrementExpression(rangeInit, rangeIncrement, parseLogicalExpr(tokens))
        else
          RangeWithIncrementExpressionUntil(rangeInit, rangeIncrement, parseLogicalExpr(tokens))
      case rangeInit =>
        if (include)
          RangeExpression(rangeInit, parseLogicalExpr(tokens))
        else
          RangeExpressionUntil(rangeInit, parseLogicalExpr(tokens))
    }
  }

  def parseListGuard(tokens:TokenStream) : ListGuard = {
    if (tokens.peek(LPAREN) && tokens.peek(2, classOf[ID]) && tokens.peek(3, COMMA)) {
      tokens.consume(LPAREN)
      var listOfIds = List.empty[String]
      val id = tokens.consume(classOf[ID])
      listOfIds = id.value :: listOfIds
      while (tokens.peek(COMMA)) {
        tokens.consume(COMMA)
        val id = tokens.consume(classOf[ID])
        listOfIds = id.value :: listOfIds
      }
      tokens.consume(RPAREN)
      tokens.consume(BACK_ARROW)
      val expr = parsePipedExpr(tokens)
      ListGuardDeclTupled(listOfIds.reverse, expr)
    }
    else if (tokens.peek(classOf[ID]) && tokens.peek(2, BACK_ARROW)) {
      val id = tokens.consume(classOf[ID])
      tokens.consume(BACK_ARROW)
      val expr = parsePipedExpr(tokens)
      ListGuardDecl(id.value, expr)
    } else {
      ListGuardExpr(parsePipedExpr(tokens))
    }
  }

  def parseSetExpr(tokens:TokenStream): Expression = {
    tokens.consume(HASHLCURLY)
    var listOfValues = List.empty[Expression]
    val value = parseExpr(tokens)
    listOfValues = value :: listOfValues
    while (tokens.peek(COMMA)) {
      tokens.consume(COMMA)
      tokens.consumeOptionals(NL)
      val value = parseExpr(tokens)
      listOfValues = value :: listOfValues
    }
    tokens.consume(RCURLY)
    SetExpression(listOfValues.reverse)
  }

  def parseDictionaryExpr(tokens:TokenStream) : Expression = {
    tokens.consume(LCURLY)
    var listOfPairs = List.empty[(Expression, Expression)]
    val key = parseKeyExpr(tokens)
    val value = parseExpr(tokens)
    listOfPairs = (key, value) :: listOfPairs
    while (tokens.peek(COMMA)) {
      tokens.consume(COMMA)
      tokens.consumeOptionals(NL)
      val key = parseKeyExpr(tokens)
      val value = parseExpr(tokens)
      listOfPairs = (key, value) :: listOfPairs
    }
    tokens.consume(RCURLY)
    DictionaryExpression(listOfPairs.reverse)
  }

  def parseKeyExpr(tokens:TokenStream) : Expression = {
    if (tokens.peek(classOf[ATOM]))
      parseAtom(tokens)
    else
      parseLiteral(tokens)
  }
}
