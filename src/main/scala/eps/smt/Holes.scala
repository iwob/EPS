package eps.smt

import swim.tree.Op
import scala.collection.mutable.MutableList
import eps.utils.EPSUtils
import swim.Grammar
import eps.core.VarDef
import eps.core.ProblemDefinition
import eps.core.FitnessEPS



class HoleDef(val name: Symbol, val tpe: String, val grammar: String = "") {
  def isGrammarDefined = grammar != ""
  def isVarHole = false
  def isConstHole = false
  /**
   * Returns string of this constant symbol definition in the format: NAME:TYPE.
   */
  def format(): String = toString()
  override def toString() = name.name + ":" + tpe
}

object HoleDef {
  def apply(name: Symbol, tpe: String, grammar: String = ""): HoleDef = new HoleDef(name, tpe, grammar)
}



/**
 * Definition of a hole which represents a variable.
 */
class HoleVarDef(name: Symbol, tpe: String, val varsNames: List[String])
                extends HoleDef(name, tpe, HoleVarDef.createGrammar(tpe, varsNames)) {
  override def isVarHole = true
}

object HoleVarDef {
  def apply(name: Symbol, tpe: String, varsNames: List[String]): HoleVarDef = new HoleVarDef(name, tpe, varsNames)
  def createGrammar(tpe: String, varsNames: List[String]) = s"((Start ${tpe} (${varsNames.mkString(" ")})))"
}



/**
 * Definition of a hole which represents a constant.
 */
class HoleConstDef(name: Symbol, tpe: String) extends HoleDef(name, tpe, "") {
  override def isConstHole = true
}

object HoleConstDef {
  def apply(name: Symbol, tpe: String) = new HoleConstDef(name, tpe)
}



/**
 * By holes we mean fragments of the tree which are currently left undefined. Concrete
 * values of these fragments will be determined by SMT solver. Computed values may be
 * put into the tree (done by fillHoles method) or only used to compute fitness.
 */
object Holes {
  /**
   * Assigns unique name to every symbolic constant found in the tree.
   * Returns a tuple containing a tree with renamed symbolic constants and a sequence of their names and types.
   * @param op program tree.
   * @param holes definitions of hole terminals provided by a user.
   */
  def renameHoles(op: Op, holes: Seq[HoleDef]): (Op, Seq[(String, HoleDef)]) = {
    val usesMap = collection.mutable.Map[Symbol,Int]() // keeps track of number of uses of each hole.
    holes.foreach { h => usesMap.put(h.name, 0) } // initialization.
    val namesList = MutableList[(String,HoleDef)]()
    def renameHelper(op: Op): Op = {
      if (op.isLeaf && op.op.isInstanceOf[Symbol]) {
        val hole = holes.find(_.name == op.op.asInstanceOf[Symbol])
        if (hole.isDefined) {
          val symb = op.op.asInstanceOf[Symbol]
          val symbNum = usesMap(symb)
          usesMap.put(symb, symbNum+1)
          val newSymb = Symbol(symb.name + symbNum)
          namesList += ((newSymb.name, hole.get))
          Op(op.nt, newSymb)
        }
        else op // not a hole.
      }
      else if (op.isLeaf)
        op
      else {
        val newArgs = op.args.map(renameHelper(_))
        op.setArgs(newArgs)
      }
    }
    val newOp = renameHelper(op)
    (newOp, namesList.toList)
  }
  
  /**
   * Substitutes every occurrence of a hole with its value in the model.
   * @param op program tree.
   * @param model values associated with holes. If model does not contain value for
   * a certain hole it assumed that any value from the hole's domain may be used.
   * @param usedHoles list of tuples containing name and type of every hole
   * present in the op.
   * @return op with holes substituted for their values in the model.
   */
  def fillHoles(op:Op,
                solverRes: SolverResult,
                usedHoles: Seq[(String, HoleDef)],
                grammar: Option[Grammar] = None,
                varDefs: Option[List[VarDef]] = None): Op = {
    // clean must be used for example when Z3 returns negative number in the model,
    // because it is returned in the form: -(x).
    def getDefaultNtForTpe(tpe: String): Symbol = if (tpe == "Int" || tpe == "Double") 'S else 'SB
    def clean(s: String) = s.replace("(", "").replace(")", "").replace(" ", "")
    def getNewOpForConstHole(usedHoleId: String, holeDef: HoleDef): Op = {
      val v = solverRes.model.get.get(usedHoleId)
      try {
        if (v.isDefined)
          Op(op.nt, EPSUtils.strToInstance(clean(v.get), holeDef.tpe))
        else
          Op(op.nt, EPSUtils.defaultValue(holeDef.tpe))
      } catch {
        case e: Throwable =>
          // println("Warning: " + e.getMessage)
          val nt = getDefaultNtForTpe(holeDef.tpe)
          Op(nt, holeDef.name)
      }
    }
    def getNewOpForGrammarHole(usedHoleId: String, holeDef: HoleDef): Op = {
      val content = solverRes.holesContent.get.get(usedHoleId)
      if (content.isDefined)
        if (content.get == usedHoleId) {
          // The case when PySV returns hole's ID as a hole's content (this means hole's content has no impact on the semantic)
          val nt = getDefaultNtForTpe(holeDef.tpe)
          if (holeDef.isVarHole) {
            val defOp = Symbol(holeDef.asInstanceOf[HoleVarDef].varsNames(0))
            Op(nt, defOp) // Put the first variable from the list as default.
          }
          else
            Op(nt, holeDef.name)
        }
        else
          SmtlibUtils.smtlibToOp(content.get, grammar, varDefs)
      else
        throw new Exception("Hole's id is missing in the output of the solver!")
    }
    def getNewOp(usedHoleId: String, holeDef: HoleDef/*k: String, tpe: String*/): Op = {
      if (!holeDef.isGrammarDefined) // simple constant-hole realized as free variable.
        getNewOpForConstHole(usedHoleId, holeDef)
      else
        getNewOpForGrammarHole(usedHoleId, holeDef)
    }
    if (op.isLeaf && op.op.isInstanceOf[Symbol]) {
      val usedHole = usedHoles.find(_._1 == op.op.asInstanceOf[Symbol].name)
      if (usedHole.isDefined)
        getNewOp(usedHole.get._1, usedHole.get._2)
      else op
    }
    else if (op.isLeaf) op // leaf other than a hole.
    else op.setArgs(op.args.map(fillHoles(_, solverRes, usedHoles, grammar, varDefs)))
  }
  
  /**
   * Fills holes in the op based on its fitness and computed model. Before filling holes with their
   * content all holes in the op will be appropriately renamed.
   */
  def fillHolesInOp(op: Op, fit: FitnessEPS, problem: ProblemDefinition): Op = {
    if (fit.solverRes.isDefined &&
        fit.solverRes.get.isSat) {
      val (renamedOp, holeNames) = Holes.renameHoles(op, problem.holesDefs)
      // println("Op before fillHoles: " + op)
      val filledOp = Holes.fillHoles(renamedOp, fit.solverRes.get, holeNames, Some(problem.grammar), Some(problem.inputVars))
      // println("Op after fillHoles: " + filledOp)
      filledOp
    }
    else op
  }
}