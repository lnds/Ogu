package lexer

import java.io.{File, FileInputStream, InputStream}
import org.joda.time.DateTime
import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

class Lexer {

  type OptToken = Option[SYMBOL]
  type TokList = List[SYMBOL]
  type TokenList = List[Token]
  type OptTokList = List[Option[SYMBOL]]
  type IntList = List[Int]

  private[this] val currentString = new StringBuilder()

  private[this] def parseMultiLineString: Boolean = currentString.nonEmpty

  def scanLine(line: String, lineNum: Int, indentStack: IntList, parenLevel: Int): (TokenList, IntList, Int) = {
    val text = line.substring(0, findCommentPos(line, pos = 0, insideString = false))
    val (str, indents, newStack) = if (parenLevel > 0) (text, Nil, indentStack) else scanIndentation(text, indentStack)
    val (tokens, newParenLevel) = splitLine(str, lineNum, parenLevel)
    val result = if (text.isEmpty || tokens.isEmpty || newParenLevel > 0 || parseMultiLineString)
      indents ++ tokens
    else
      indents ++ tokens ++ List(Some(NL))
    (result.flatten.map(Token(_, lineNum + 1)), newStack, newParenLevel)
  }

  private[this] def scanIndentation(text: String, indents: IntList): (String, OptTokList, IntList) = {
    val s = text.dropWhile(OperatorMap.isBlank)
    val startColumn = text.length - s.length
    indents.headOption match {
      case None => (s, Nil, indents)
      case Some(pos) =>
        pos match {
          case _ if startColumn > pos =>
            (s, List(Some(INDENT)), startColumn :: indents)
          case _ if startColumn < pos =>
            val newIndentStack = indents.dropWhile(p => startColumn < p && p > 0)
            (s, List.fill(indents.length - newIndentStack.length)(Some(DEDENT)), newIndentStack)
          case _ =>
            (s, Nil, indents)
        }
    }
  }

  @tailrec
  private[this] def findCommentPos(line: String, pos: Int, insideString: Boolean): Int = {
    line.headOption match {
      case None => pos
      case Some(c) =>
        if (line.startsWith("--") && !insideString)
          pos
        else {
          c match {
            case '\\' => findCommentPos(line.drop(2), pos + 2, insideString)
            case '"' => findCommentPos(line.tail, pos + 1, !insideString)
            case _ => findCommentPos(line.tail, pos + 1, insideString)
          }
        }
    }
  }

  private[this] def splitLine(txt: String, currentLine: Int, parenLevel: Int): (OptTokList, Int) = {
    val len = txt.length
    val (tokLst, newParenLevel, ini, pos) = scanTokens(txt, txt, currentLine, parenLevel, Nil, 0, 0)
    if (ini < len) {
      val (token, level) = strToToken(txt.substring(ini, pos), currentLine, newParenLevel)
      ((token :: tokLst).reverse, level)
    }
    else {
      (tokLst.reverse, newParenLevel)
    }
  }

  @tailrec
  private[this]
  def scanTokens(txt: String, str: String, cl: Int, pl: Int, tokens: OptTokList, ini: Int, pos: Int): (OptTokList, Int, Int, Int) = {
    str.headOption match {
      case None => (tokens, pl, ini, pos)
      case Some(c) =>
        c match {
          case '\"' =>
            val (r, npl) = checkToken(txt, ini, pos, cl, pl, tokens)
            val (r2, ip, npl2) = addQuoted(txt, str, ini, pos, cl, npl, r)
            scanTokens(txt, str.drop(ip - pos), cl, npl2, r2, ip, ip)
          case '\'' =>
            if (pos > ini || pos < ini)
              scanTokens(txt, str.drop(1), cl, pl, tokens, ini, pos + 1)
            else {
              val (r, ip, npl) = addQuoted(txt, str, ini, pos, cl, pl, tokens)
              scanTokens(txt, str.substring(ip - pos), cl, npl, r, ip, ip)
            }
          case '.' if str.startsWith("...") || str.startsWith("..<") =>
            val (r, npl) = checkToken(txt, ini, pos, cl, pl, tokens)
            val r2 = OperatorMap(txt.substring(pos, pos + 3)) :: r
            scanTokens(txt, str.drop(3), cl, npl, r2, pos + 3, pos + 3)

          case '.' if str.startsWith("..") =>
            val (r, npl) = checkToken(txt, ini, pos, cl, pl, tokens)
            scanTokens(txt, str.drop(2), cl, npl, Some(DOTDOT) :: r, pos + 2, pos + 2)

          case '#' =>
            scanHash(txt, str, cl, pl, tokens, ini, pos)

          case _ if OperatorMap.isBlank(c) =>
            val (r, npl) = checkToken(txt, ini, pos, cl, pl, tokens)
            val newPos = skip(pos, txt.substring(pos), OperatorMap.isBlank)
            scanTokens(txt, str.drop(newPos - pos), cl, npl, r, newPos, newPos)

          case _ if OperatorMap.isPunct(c) =>
            val (r, npl) = checkToken(txt, ini, pos, cl, pl, tokens)
            val (token, level) = tryParseOp(txt.substring(pos, pos + 1), cl, npl)
            scanTokens(txt, str.drop(1), cl, level, token :: r, pos + 1, pos + 1)

          case _ =>
            scanTokens(txt, str.drop(1), cl, pl, tokens, ini, pos + 1)
        }
    }
  }

  private[this]
  def scanHash(txt:String, str:String,  cl: Int, pl: Int, tokens: OptTokList, ini:Int, pos: Int): (OptTokList, Int, Int, Int)  = {
    val (r, npl) = checkToken(txt, ini, pos, cl, pl, tokens)
    val newTxt = str.drop(1)
    newTxt.headOption match {
      case None => scanTokens(txt, newTxt, cl, npl, r, ini, pos + 1)
      case Some(c) =>
        c match {
          case '\"' =>
            val (r2, ip, npl2) = addQuoted(txt, str, pos, pos + 1, cl, npl, r)
            scanTokens(txt, str.drop(ip - pos), cl, npl2, r2, ip, ip)
          case '/' =>
            val (quotedStr, newIni) = parseQuoted('/', txt, pos, pos + 1)
            val (token, level) = parseHashTag(quotedStr, cl, npl)
            scanTokens(txt, str.drop(newIni - pos), cl, level, token :: r, newIni, newIni)
          case '{' =>
            val (token, level) = parseHashTag(txt.substring(ini, pos + 2), cl, npl)
            scanTokens(txt, str.drop(2), cl, level, token :: r, pos + 2, pos + 2)
          case _ if OperatorMap.isTimeValidChar(c) =>
            val newPos = skip(pos + 1, newTxt, OperatorMap.isTimeValidChar)
            val (token, level) = parseHashTag(txt.substring(ini, newPos), cl, npl)
            scanTokens(txt, str.drop(newPos - pos), cl, level, token :: r, newPos, newPos)
          case _ =>
            scanTokens(txt, newTxt, cl, npl, r, ini, pos + 1)
        }
    }
  }

  private[this] def parseQuoted(quot: Char, txt: String, ini: Int, oldPos: Int): (String, Int) = {
    val pos = findQuot(Set(quot), txt.substring(oldPos+1), oldPos+1)
    (txt.substring(ini, pos), pos)
  }

  @tailrec
  private[this] def findQuot(quot: Set[Char], str: String, pos: Int) : Int = {
    str.headOption match {
      case None => pos
      case Some(c) =>
        c match {
          case q if quot contains q => pos+1
          case '\\' => findQuot(quot, str.drop(2), pos+2)
          case _ => findQuot(quot, str.tail, pos+1)
        }
    }
  }

  private[this]
  def addQuoted(txt:String, str: String, ini: Int, pos: Int, cl: Int, pl: Int, tokens: OptTokList): (OptTokList, Int, Int) = {
    val (quotedStr, newIni) = parseQuoted(str.head, txt, ini, pos)
    val (token, level) = strToToken(quotedStr, cl, pl)
    (token::tokens, newIni, level)
  }

  private[this]
  def checkToken(txt: String, ini: Int, pos: Int, cl: Int, pl: Int, lst:OptTokList): (OptTokList, Int) = {
    if (pos > ini) {
      addToken(txt.substring(ini, pos), cl, pl, lst)
    } else {
      (lst, pl)
    }
  }

  private[this]
  def addToken(str:String, currentLine: Int, parenLevel: Int, tokens: OptTokList) : (OptTokList, Int) = {
    val (token, level) = strToToken(str, currentLine, parenLevel)
    (token :: tokens, level)
  }

  @tailrec
  private[this] def skip(pos: Int, str: String, f: Char => Boolean): Int = {
    str.headOption match {
      case None => pos
      case Some(c) if f(c) => skip(pos+1, str.tail, f)
      case _ => pos
    }
  }

  private[this]
  def strToToken(str: String, currentLine: Int, parenLevel: Int): (OptToken, Int) = {
    if (parseMultiLineString) {
      currentString ++= str
      if (!str.endsWith("\"")) {
        (None, parenLevel)
      }
      else {
        val str = currentString.mkString
        currentString.clear()
        (Some(STRING(str)), parenLevel)
      }
    }
    else {
      str.headOption match {
        case None => (Some(ERROR(currentLine, str)), parenLevel)
        case Some(c) =>
          c match {
            case ':'=> parseColonToken(str, parenLevel)
            case '#' => parseHashTag(str, currentLine, parenLevel)
            case '\"' if !str.endsWith("\"") =>
              currentString ++= str
              (None, parenLevel)
            case '\"' => (Some(STRING(str)), parenLevel)
            case '\'' => (Some(CHAR(str)), parenLevel)
            case _ =>
              val token1 = tryParseId(str, currentLine)
              val token2 = token1 match {
                case Some(ERROR(_, _)) => tryParseNum(str, currentLine)
                case _ => token1
              }
              token2 match {
                case Some(ERROR(_, _)) => tryParseOp(str, currentLine, parenLevel)
                case _ => (token2, parenLevel)
              }
          }
      }
    }
  }

  private[this] def parseColonToken(str: String, parenLevel: Int): (OptToken, Int) = {
    str match {
      case ":" => (OperatorMap(str), parenLevel)
      case "::" => (OperatorMap(str), parenLevel)
      case _ => (Some(ATOM(str)), parenLevel)
    }
  }

  private[this] def tryParseId(str: String, currentLine: Int): OptToken = {
    KeywordMap(str) match {
      case Some(token) => Some(token)
      case None =>
        val s = str.takeWhile(c => !OperatorMap.isIdentifierChar(c))
        if (s.length >= str.length) {
          Some(ERROR(currentLine, str))
        }
        else {
          val id = if (str.endsWith("...")) str.substring(0, str.length - 3) else str
          id.headOption match {
            case None => Some(ERROR(currentLine, str))
            case Some(c) if !id.contains('.') && c.isUpper => Some(TID(id))
            case _ if id.contains('.') =>
              val parts = id.split('.')
              parts.lastOption match {
                case None => Some(ID(id))
                case Some(last) =>
                  last.headOption match {
                    case None => Some(ID(id))
                    case Some(c) if c.isUpper => Some(TID(id))
                    case _ => Some(ID(id))
                  }
              }
            case _ => Some(ID(id))
          }
        }
    }
  }

  private[this] def isIntegerValue(bd: BigDecimal): Boolean = {
    bd.signum match {
      case 0 => true
      case _ => bd.scale <= 0
    }
  }

  private[this] def strToNumToken(str: String): SYMBOL = {
    val value = BigDecimal(str)
    if (isIntegerValue(value)) {
      value match {
        case i if i < Int.MaxValue => INT(value.toIntExact)
        case l if l < Long.MaxValue => LONG(value.toLongExact)
        case _ => BIGINT(value.toBigInt())
      }
    }
    else {
      value match {
        case d if d.isExactDouble => DOUBLE(value.toDouble)
        case l if l.isValidLong => LONG(value.toLongExact)
        case b if b.isBinaryFloat => FLOAT(value.toFloat)
        case _ => BIGDECIMAL(value)
      }
    }
  }

  private[this] def tryParseNum(str: String, currentLine: Int): OptToken = {
    Try(strToNumToken(str)) match {
      case Success(token) => Some(token)
      case Failure(_) => Some(ERROR(currentLine, str))
    }
  }

  private[this] def tryParseOp(str: String, currentLine: Int, parenLevel: Int): (OptToken, Int) = {
    OperatorMap(str) match {
      case Some(token) =>
        val newLevel = token match {
          case LPAREN | LBRACKET | LCURLY | HASHLCURLY => parenLevel + 1
          case RPAREN | RBRACKET | RCURLY => parenLevel - 1
          case _ => parenLevel
        }
        (Some(token), newLevel)
      case None =>
        (Some(ERROR(currentLine, str)), parenLevel)
    }
  }

  private[this] def parseHashTag(str: String, currentLine: Int, parenLevel: Int): (OptToken, Int) = {
    str match {
      case "#{" =>
        (Some(HASHLCURLY), parenLevel + 1)
      case _ if str.startsWith("#\"") =>
        (Some(FSTRING(str.substring(1))), parenLevel)
      case _ if str.startsWith("#/") =>
        (Some(REGEXP(str.substring(2, str.length - 1))), parenLevel)
      case _ =>
        Try(ISODATETIME(new DateTime(str.substring(1)))) match {
          case Success(token) => (Some(token), parenLevel)
          case Failure(_) => (Some(ERROR(currentLine, str)), parenLevel)
        }
    }
  }


  @tailrec
  private[this]
  def mapLines(lines: List[(String, Int)], indentStack: IntList, tokens: List[TokenList], parenLevel: Int) : (TokenList, IntList) = {
    lines.headOption match {
      case None => (tokens.reverse.flatten, indentStack)
      case Some((text, line)) =>
        val (lineTokens, newIndentStack, newParenLevel) = scanLine(text, line, indentStack, parenLevel)
        mapLines(lines.tail, newIndentStack, lineTokens :: tokens, newParenLevel)
    }
  }

  private[this] def scanLines(lines: List[(String, Int)]): TokenStream = {
    val (tokens, indentStack) = mapLines(lines, List(0), Nil, parenLevel = 0)
    val result = if (indentStack.isEmpty) {
      tokens
    } else {
      val newIndentStack = indentStack.dropWhile(p => p > 0)
      List.fill(indentStack.length - newIndentStack.length)(Token(DEDENT,lines.length+1)) ++ tokens.reverse
    }
    TokenStream(result.reverse)
  }

  def scan(filename: String, fileStream: InputStream): Try[TokenStream] = {
    Try(Source.fromInputStream(fileStream)) match {
      case Failure(e) =>
        Failure(CantScanFileException(filename, e))
      case Success(rdr) =>
        Success(scanLines(rdr.getLines.zipWithIndex.filter { case (s, _) => s.nonEmpty }.toList))
    }
  }

  def scan(filename: String): Try[TokenStream] = {
    scan(filename, new FileInputStream(new File(filename)))
  }

  def scanString(code: String): Try[TokenStream] = {
    Try {
      scanLines(code.split('\n').zipWithIndex.filter { case (s, _) => s.nonEmpty }.toList)
    }
  }

}
