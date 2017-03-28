package eps.smt


import swim.DomainWithVars
import swim.Grammar
import swim.tree.Op
import eps.core.VarDef
import eps.utils.OptionsEPS
import swim.tree.ConstantProviderUniformI
import fuel.util.TRandom
import swim.tree.ConstantProvider
import swim.tree.ConstantProviderUniformD


abstract class EPSDomain(numVars: Int) extends DomainWithVars[Seq[Any], Any, Op](numVars) {}


object LogicsCommon {
  /**
   * Returns a map from type to list of 
   */
  def getTerminalsLists(prodTypes: Map[Symbol,String], vars: List[VarDef], holes: List[HoleDef])
                       (implicit env: OptionsEPS, rng: TRandom): Map[Symbol, List[Any]] = {
    val res = prodTypes.map{ case (symb, tpe) =>
      val varsList = getVarsForType(vars, tpe)
      val ephConstsList = getEphemeralConstsForType(tpe)
      val holesList = getHolesForType(holes, tpe)
      (symb, varsList ++ ephConstsList ++ holesList)
    }
    res
  }
  
  def getHolesForType(holes: List[HoleDef], tpe: String): List[Symbol] = {
    holes.filter(_.tpe == tpe).map(_.name)
  }
  
  def getVarsForType(vars: List[VarDef], tpe: String)
                    (implicit env: OptionsEPS): List[Symbol] = {
    if (!env.getBool(OptionsEPS.useInputVarsAsTerminals))
      List()
    else
      vars.filter(_.tpe == tpe).map(_.name)
  }
  
  def getEphemeralConstsForType(tpe: String)
                               (implicit env: OptionsEPS, rng: TRandom): List[ConstantProvider] = {
    if (!env.getBool(OptionsEPS.useConstantProvider))
      List()
    else tpe match {
      case "Int" =>  List(ConstantProviderUniformI(0, 5))
      case "Double" =>  List(ConstantProviderUniformD(0.0, 5.0))
      case _ => List()
    }
  }
}



object CORE {
  class Domain(inputVars: List[VarDef]) extends EPSDomain(inputVars.size) {
    val varNames: List[Symbol] = inputVars.map(_.name)
    override def semantics(input: Seq[Any]) = {
      assume(input.size == numVars)
      new Function1[Op, Any] {
        def apply(op: Op): Any = {
          // Needs toList (otherwise ArrayBuffer, which doesn't work with patter matching)
          val childRes = op.args.toSeq.map(c => apply(c)).toList
          childRes.+:(op.op) match {
            case Seq('and, x: Boolean, y: Boolean)             => x && y
            case Seq('or, x: Boolean, y: Boolean)              => x || y
            case Seq('not, x: Boolean)                         => !x
            case Seq('ite, b: Boolean, x: Boolean, y: Boolean) => if (b) x else y
            case Seq(s: Symbol)                                => input(varNames.indexOf(s))
            case Seq(v: Boolean)                               => v
            case Seq(s: String)                                => throw new Exception("Domain: supplied String for terminal name instead of Symbol!")
          }
        }
      }
    }
  }
  
  def getDomain(inputVars: List[VarDef]): CORE.Domain = {
    new CORE.Domain(inputVars)
  }
  
  val prodTypes = Map('SB -> "Bool")
  def getGrammar(vars: List[VarDef],
                 holes: List[HoleDef],
                 env: OptionsEPS)
                (implicit rng: TRandom): Grammar = {
    val terminalsMap = LogicsCommon.getTerminalsLists(prodTypes, vars, holes)(env, rng)
    val startSymbol = 'SB
    Grammar(startSymbol,
      'SB -> (terminalsMap('SB) ++ List(
        'and -> ('S, 'S),
        'or -> ('S, 'S),
        'not -> ('S))))
  }
}



object LIA {
  class Domain(inputVars: List[VarDef]) extends EPSDomain(inputVars.size) {
    val varNames: List[Symbol] = inputVars.map(_.name)
    override def semantics(input: Seq[Any]) = {
      assume(input.size == numVars)
      new Function1[Op, Any] {
        def apply(op: Op): Any = {
          // Needs toList (otherwise ArrayBuffer, which doesn't work with patter matching)
          val childRes = op.args.toSeq.map(c => apply(c)).toList
          childRes.+:(op.op) match {
            case Seq('<, x: Int, y: Int)               => x < y
            case Seq('<=, x: Int, y: Int)              => x <= y
            case Seq('>, x: Int, y: Int)               => x > y
            case Seq('>=, x: Int, y: Int)              => x >= y
            case Seq('=, x, y)                         => x == y
            case Seq('+, x: Int, y: Int)               => x + y
            case Seq('-, x: Int, y: Int)               => x - y
            case Seq('*, x: Int, y: Int)               => x * y
            case Seq('ite, b: Boolean, x: Int, y: Int) => if (b) x else y
            case Seq(s: Symbol)                        => input(varNames.indexOf(s))
            case Seq(v: Int)                           => v
            case Seq(v: Boolean)                       => v
            case Seq(s: String)                        => throw new Exception("Domain: supplied String for terminal name instead of Symbol!")
          }
        }
      }
    }
  }
  
  def getDomain(inputVars: List[VarDef]): LIA.Domain = {
    new LIA.Domain(inputVars)
  }
  
  val prodTypes = Map('S -> "Int", 'SB -> "Bool")
  def getGrammar(vars: List[VarDef],
                 holes: List[HoleDef],
                 env: OptionsEPS)
                (implicit rng: TRandom): Grammar = {
    val terminalsMap = LogicsCommon.getTerminalsLists(prodTypes, vars, holes)(env, rng)
    val startSymbol = 'S
    Grammar(startSymbol,
      'S -> (terminalsMap('S) ++ List(
        '+ -> ('S, 'S),
        '- -> ('S, 'S),
        '* -> ('S, 'S),
        'ite -> ('SB, 'S, 'S)
      )),
      'SB -> (terminalsMap('SB) ++ List(
        '< -> ('S, 'S),
        '<= -> ('S, 'S),
        '> -> ('S, 'S),
        '>= -> ('S, 'S),
        '= -> ('S, 'S),
        '= -> ('SB, 'SB))))
  }
}



object NIA {
  class Domain(inputVars: List[VarDef], smtlibDiv: Boolean = true) extends EPSDomain(inputVars.size) {
    val varNames: List[Symbol] = inputVars.map(_.name)
    def divide(x: Int, y: Int): Int = {
      if (y == 0) throw new Exception("Division by 0 during evaluation of the solution!")
      if (smtlibDiv) {
        /* "Regardless of sign of m,
            when n is positive, (div m n) is the floor of the rational number m/n;
            when n is negative, (div m n) is the ceiling of m/n."
          Source: http://smtlib.cs.uiowa.edu/theories-Ints.shtml
        */
        val quotient = x.asInstanceOf[Double] / y
        if (y > 0) math.floor(quotient).toInt
        else math.ceil(quotient).toInt
      }
      else x / y // normal, common for programming languages division (always rounding in the direction of 0).
    }
    override def semantics(input: Seq[Any]) = {
      assume(input.size == numVars)
      new Function1[Op, Any] {
        def apply(op: Op): Any = {
          // Needs toList (otherwise ArrayBuffer, which doesn't work with patter matching)
          val childRes = op.args.toSeq.map(c => apply(c)).toList
          childRes.+:(op.op) match {
            case Seq('<, x: Int, y: Int)               => x < y
            case Seq('<=, x: Int, y: Int)              => x <= y
            case Seq('>, x: Int, y: Int)               => x > y
            case Seq('>=, x: Int, y: Int)              => x >= y
            case Seq('=, x, y)                         => x == y
            case Seq('+, x: Int, y: Int)               => x + y
            case Seq('-, x: Int, y: Int)               => x - y
            case Seq('*, x: Int, y: Int)               => x * y
            case Seq('div, x: Int, y: Int)             => divide(x, y)
            case Seq('ite, b: Boolean, x: Int, y: Int) => if (b) x else y
            case Seq(s: Symbol)                        => input(varNames.indexOf(s))
            case Seq(v: Int)                           => v
            case Seq(v: Boolean)                       => v
            case Seq(s: String)                        => throw new Exception("Domain: supplied String for terminal name instead of Symbol!")
          }
        }
      }
    }
  }
  
  def getDomain(inputVars: List[VarDef], smtlibDiv: Boolean = true): NIA.Domain = {
    new NIA.Domain(inputVars, smtlibDiv)
  }
  
  val prodTypes = Map('S -> "Int", 'SB -> "Bool")
  def getGrammar(vars: List[VarDef],
                 holes: List[HoleDef],
                 env: OptionsEPS)
                (implicit rng: TRandom): Grammar = {
    val terminalsMap = LogicsCommon.getTerminalsLists(prodTypes, vars, holes)(env, rng)
    val startSymbol = 'S
    Grammar(startSymbol,
      'S -> (terminalsMap('S) ++ List(
        '+ -> ('S, 'S),
        '- -> ('S, 'S),
        '* -> ('S, 'S),
        'div -> ('S, 'S),
        'ite -> ('SB, 'S, 'S)
      )),
      'SB -> (terminalsMap('SB) ++ List(
        '< -> ('S, 'S),
        '<= -> ('S, 'S),
        '> -> ('S, 'S),
        '>= -> ('S, 'S),
        '= -> ('S, 'S),
        '= -> ('SB, 'SB))))
  }
}
