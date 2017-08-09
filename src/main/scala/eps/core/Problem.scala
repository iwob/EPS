package eps.core

import swim.Test
import swim.Grammar
import eps.smt.LIA
import eps.smt.EPSDomain
import eps.utils.EPSUtils
import eps.utils.OptionsEPS
import fuel.util.TRandom
import eps.smt.NIA
import eps.smt.CORE
import eps.smt.HoleConstDef
import eps.smt.HoleDef
import eps.smt.HoleVarDef




/**
 * Definition of a variable.
 */
class VarDef(val name: Symbol, val tpe: String) {
  /**
   * Returns string of this variable definition in the format: NAME:TYPE.
   */
  def format(): String = toString()
  override def toString: String = name.name + ":" + tpe
}

object VarDef {
  def apply(name: Symbol, tpe: String) = new VarDef(name, tpe)
}



/**
 * Stores data of a certain test-based synthesis problem/benchmark.
 */
class ProblemDefinition(val tests: List[Test[List[Any],Any]],
                        val inputVars: List[VarDef],
                        val outputVars: List[VarDef],
                        val logic: String,
                        val domain: EPSDomain,
                        val grammar: Grammar,
                        val holesDefs: List[HoleDef],
                        val env: OptionsEPS) {
  lazy val inputVarsNames: Seq[String] = inputVars.map(_.name.name)
  lazy val outputVarsNames: Seq[String] = outputVars.map(_.name.name)
  lazy val symbConstsNames: Seq[String] = holesDefs.map(_.name.name)
}

object ProblemDefinition {
  def apply(env: OptionsEPS)(implicit rng: TRandom = null): ProblemDefinition = {
    val path = env.getOption(OptionsEPS.pathTests).get
    val (tests, inputVars, outputVars) = EPSUtils.loadTestsFile(path)
    apply(env, tests, inputVars, outputVars)
  }
  def apply(env: OptionsEPS,
            tests: List[Test[List[Any],Any]],
            inputVars: List[VarDef],
            outputVars: List[VarDef])
           (implicit rng: TRandom): ProblemDefinition = {
    val logic = env.getOption(OptionsEPS.logic).get
    val holesDefs = getHolesDefs(env)
    val (domain, grammar) = logic match {
        case "CORE"           => (CORE.getDomain(inputVars), CORE.getGrammar(inputVars, holesDefs, env))
        case "LIA" | "QF_LIA" => (LIA.getDomain(inputVars), LIA.getGrammar(inputVars, holesDefs, env))
        case "NIA" | "QF_NIA" => (NIA.getDomain(inputVars), NIA.getGrammar(inputVars, holesDefs, env))
        case "NRA" | "QF_NRA" => ???
      }
    if (!env.getBool(OptionsEPS.silent)) {
      printTests(tests)
      println("INPUT VARS: " + inputVars.mkString(", "))
      println("OUTPUT VARS: " + outputVars.mkString(", "))
    }
    new ProblemDefinition(tests, inputVars, outputVars, logic, domain, grammar, holesDefs, env)
  }
  def getHolesDefs(env: OptionsEPS): List[HoleDef] = {
    if (env.getBool(OptionsEPS.enableHoles)) {
      val holesConsts = getHolesConsts(env.getOption(OptionsEPS.holesConsts).get)
      val holesVars = getHolesVarsDefs(env.getOption(OptionsEPS.holesVars).get)
      holesConsts ++ holesVars
    }
    else List()
  }
  def printTests(tests: List[Test[List[Any],Any]]) {
    println("\nTESTS:")
    tests.foreach{ t => println(t.input.mkString("[", ", ", "]") + " --> " + t.output) }
  }
  
  def getHolesConsts(s: String): List[HoleConstDef] = {
    if (s == "") List()
    else s.split(";").map{ s => val w = s.split(":")
        new HoleConstDef(Symbol(w(0)), w(1))
      }.toList
  }
  def getVars(s: String): List[VarDef] = {
    s.split(",").map{ s => val w = s.split(":")
      new VarDef(Symbol(w(0)), w(1))
    }.toList
  }
  def getHolesVarsDefs(s: String): List[HoleVarDef] = {
    if (s == "") List()
    else s.split(";").toList.map { s =>
        val w = s.split(":")
        val varsNames = w(2).split(",").toList
        new HoleVarDef(Symbol(w(0)), w(1), varsNames)
      }
  }
}