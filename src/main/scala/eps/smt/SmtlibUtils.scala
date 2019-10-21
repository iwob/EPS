package eps.smt

import swim.tree.Op
import swim.Grammar
import eps.core.VarDef


object SmtlibUtils {
  /**
   * Returns SMT-LIB 2.0 code of the expression represented by op.
   */
  def opToSmtlib(op: Op): String = {
    if (op.op == 'symbConst)
      op.op.asInstanceOf[Symbol].name
    else if (op.isLeaf)
      op.toString()
    else
      "(" + op.op.asInstanceOf[Symbol].name + " " + op.args.map(opToSmtlib(_)).mkString(" ") + ")"
  }
  
  /**
   * Returns a list of SMT-LIB 2.0 assertions for all divisors in the op to be different than 0.
   */
  def getDivSafetyConditions(op: Op): List[String] = {
    def getAssertionNeq0(op: Op): String = {
      s"(assert (not (= 0 ${SmtlibUtils.opToSmtlib(op)})))"
    }
    def excludeFromSafetyConds(op: Op): Boolean = op.op.isInstanceOf[Int] || op.op.isInstanceOf[Double]
    def helperDiv(a: Op, b: Op): List[String] = {
      if (!excludeFromSafetyConds(b))
          getAssertionNeq0(b) :: (getDivSafetyConditions(a) ++ getDivSafetyConditions(b))
      else
        getDivSafetyConditions(a) ++ getDivSafetyConditions(b)
    }
    if (op.isLeaf) List()
    else op match {
      case Op(_, 'div, a, b) => /*println("div");*/ helperDiv(a, b)
      case Op(_, '/, a, b) => /*println("/");*/ helperDiv(a, b)
      case _ => op.args.map(getDivSafetyConditions(_)).flatten.toList
    }
  }
  
  /**
   * Constructs Op given its string encoding in the form: Op(ARG1, ARG2, ...).
   * As nonterminal symbol assigned will be 'default.
   * For example from "+(-(a, b), c)" will be created Op('+, Op('-, Op('a), Op('b)), Op('c)).
   * 
   * @param s string encoding of op.
   * @param convertConsts if set to true (default), terminals detected as Boolean, Int, Double or
   * String constants will be converted to instances of those types.
   * @param delim delimiter which separates arguments of functions (default: " ").
   */
  def smtlibToOp(s: String, grammar: Option[Grammar] = None, varDefs: Option[List[VarDef]] = None, delim: String = "\\s+", convertConsts: Boolean = true): Op = {
    def isBoolean(s: String): Boolean = if (s == "true" || s == "false") true else false
    def isInt(s: String): Boolean = try { val x = s.toInt; true } catch { case _:Throwable => false }
    def isDouble(s: String): Boolean = try { val x = s.toDouble; true } catch { case _:Throwable => false }
    def isString(s: String): Boolean = if (s.head == '\"' && s.last == '\"') true else false
    def getTerminalOp(s: String): Any = {
      if (convertConsts)
        if (isBoolean(s)) s.toBoolean
        else if (isInt(s)) s.toInt
        else if (isDouble(s)) s.toDouble
        else if (isString(s)) s.substring(1, s.size-1)
        else Symbol(s)
      else
        Symbol(s)
    }
    def getNt(symb: Symbol): Symbol = {
      if (grammar.isDefined) {
        val nt = getNtOfGrammarSymbol(grammar.get, symb)
        if (nt.isDefined) nt.get // Variable was in the grammar and we can return production symbol.
        else { // Variable was not in the grammar - we will now check variable definitions if they were provided.
          if (varDefs.isDefined) {
            val v = varDefs.get.find(_.name == symb)
            if (v.isDefined) {
              v.get.tpe match {
                case "Int" | "Double" => 'S // Hardcoded and hard to fix...
                case "Boolean" => 'SB // Hardcoded and hard to fix...
                case _ => 'default
              }
            }
            else
              'default
          }
          else
            'default // There is no other choice as returning default production symbol.
        }
      }
      else 'default
    }
    def getNtForTerminal(value: Any): Symbol = {
      if (grammar.isDefined)
        value match {
          case x: Int => 'S // Hardcoded and hard to fix...
          case x: Double => 'S // Hardcoded and hard to fix...
          case x: Boolean => 'SB // Hardcoded and hard to fix...
          case x: Symbol => getNt(x)
          case x: Any => 'default
        }
      else 'default
    }
    def getMatchingParenthIndex(words: Array[String], begin: Int): Int = {
      var parOpened = 1
      for (i <- (begin+1) until words.size) {
        if (words(i) == ")") parOpened -= 1
        else if (words(i) == "(") parOpened += 1
        if (parOpened == 0)
          return i
      }
      words.size
    }
    def getArgs(words: Array[String]): List[Op] = {
      var i = 0
      var args = List[Op]()
      while (i < words.size) {
        if (words(i) != "(") {
          val value = getTerminalOp(words(i))
          val nt = getNtForTerminal(value)
          args = args :+ Op(nt, value)
          i += 1
        }
        else {
          val matchParIndex = getMatchingParenthIndex(words, i)
          val text = words.slice(i, matchParIndex+1).mkString(" ")
          args = args :+ smtlibToOp(text, grammar, varDefs, delim, convertConsts)
          i = matchParIndex + 1
        }
      }
      args
    }
    try {
      val words = s.replace("(", " ( ").replace(")", " ) ").split(delim).filter(!_.isEmpty())
      if (words.head != "(") {
        val value = getTerminalOp(words.head)
        val nt = getNtForTerminal(value)
        Op(nt, value) // Returning terminal.
      }
      else {
        val op = words(1)
        val args = getArgs(words.slice(2, words.size-1))
        Op(getNt(Symbol(op)), Symbol(op), args:_*)
      }
    } catch {
      case _:Throwable => throw new Exception(s"Wrong encoding of Op instance: $s!")
    }
  }
  
  /**
   * Returns symbol of a grammar rule containing element with symbol 'symb'.
   * This method is used for example during creating op from smtlib (nt field of Op).
   */
  def getNtOfGrammarSymbol(grammar: Grammar, symb: Symbol): Option[Symbol] = {
    var nt: Option[Any] = None
    grammar.allProductions.foreach{ case (k, p) =>
      val ntCand = p.right.find {
        case (op: Any, args: Seq[Any]) => op == symb
        case x: Any => x == symb
      }
      if (ntCand.isDefined)
        nt = Some(k.asInstanceOf[Symbol])
    }
    if (nt.isDefined) Some(nt.get.asInstanceOf[Symbol])
    else None
  }
}