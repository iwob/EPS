package eps.smt

import scala.sys.process._
import eps.core.ProblemDefinition
import eps.utils.OptionsEPS
import swim.Test
import swim.tree.Op


/**
 * Is responsible for calling PySV framework. The data needed by solver and those independent
 * from the op are generated only once during initialization of the class instance.
 */
class RunnerPySV(val problem: ProblemDefinition) {
  val commonParams = PySV.getCommonParams(problem)
  val pathToPySV = problem.env.getString(OptionsEPS.pathToPySV)
  
  /**
   * Synthesizes contents of the holes in the provided op.
   * @param op expression tree of a SMT-LIB program.
   * @param holesNames information about holes present in the expression tree. They are provided as a tuple containing
   * name and hole definition.
   */
  def runSynthesis(op: Op, holesNames: Seq[(String, HoleDef)]): SolverResult = {
    val safetyConditions = getSafetyConditions(op)
    val pProgram = "(= res " + SmtlibUtils.opToSmtlib(op) + ")"
    val pLocalVars = getLocalVars(holesNames)
    val holesVarsDefs = PySV.getHolesDeclsText(holesNames)
    val holesConstsNames = holesNames.filter(!_._2.isGrammarDefined).map(_._1)
    val cmd = Seq("python", pathToPySV, "--synthesize", 
                  "--synth_mode", "normal",
                  "--program", pProgram,
                  "--synth_holes", holesVarsDefs) ++
              Seq("--local_vars") ++ pLocalVars ++
              Seq("--free_vars") ++ holesConstsNames ++
              Seq("--assertions") ++ safetyConditions ++
              commonParams
    PySV.runSolver(cmd)
  }
  
  /**
   * Synthesizes contents of the holes in the provided op while trying to maximize the number of passed test cases.
   * @param op expression tree of a SMT-LIB program.
   * @param holesNames information about holes present in the expression tree. They are provided as a tuple containing
   * name and hole definition.
   */
  def runSynthesisOpt(op: Op, holesNames: Seq[(String, HoleDef)], minPassedTests: Option[Int] = None): SolverResult = {
    val safetyConditions = getSafetyConditions(op)
    val pProgram = "(= res " + SmtlibUtils.opToSmtlib(op) + ")"
    val pLocalVars = getLocalVars(holesNames)
    val holesVarsDefs = PySV.getHolesDeclsText(holesNames)
    val holesConstsNames = holesNames.filter(!_._2.isGrammarDefined).map(_._1)
    val cmd = Seq("python", pathToPySV, "--synthesize",
                  "--synth_mode", "max",
                  "--produce_assignments", "1",
                  "--program", pProgram,
                  "--synth_holes", holesVarsDefs) ++
               Seq("--local_vars") ++ pLocalVars ++
               Seq("--free_vars") ++ holesConstsNames ++
               Seq("--assertions") ++ safetyConditions ++
               commonParams ++
               (if (minPassedTests.isDefined) Seq("--synth_min_passed_tests", minPassedTests.get.toString()) else Seq())
    PySV.runSolver(cmd)
  }
  
  def getLocalVars(holesNames: Seq[(String, HoleDef)]): List[String] = {
    (problem.outputVars.map(_.format) ++ holesNames.filter(!_._2.isGrammarDefined).map{ case (id, h) => id+":"+h.tpe})
  }
  
  def getSafetyConditions(op: Op): List[String] = {
    if (problem.env.getBool(OptionsEPS.safetyConditions)) SmtlibUtils.getDivSafetyConditions(op)
    else List()
  }
}


object RunnerPySV {
  def apply(problem: ProblemDefinition) = new RunnerPySV(problem)
}



object PySV {
  val pSolverOutData = List("decision", "model", "assignments", "holes_content")
  def getCommonParams(problem: ProblemDefinition): Seq[String] = {
    def solverTimeout = problem.env.getInt(OptionsEPS.solverTimeout)
    def inputVars = problem.inputVars.map(_.format)
    def pyTests = PySV.tests2py(problem.tests, problem.inputVarsNames, problem.outputVarsNames)
    val params = Seq("--lang", "smt2",
                     "--pre", "true",
                     "--post", "true",
                     "--solver_timeout", solverTimeout.toString,
                     "--logic", problem.logic,
                     "--silent", "1",
                     "--produce_proofs", "0",
                     "--save_script_to_file", "0",
                     "--input_vars") ++ inputVars ++
                     Seq("--test_cases", pyTests) ++
                     Seq("--output_data") ++ pSolverOutData
    params
  }
  
  def runSolver(cmd: Seq[String]): SolverResult = {
    val result = cmd.!!
    // println("solverOutput: " + result)
    SolverResult(result, pSolverOutData)
  }
  
  def getHolesDeclsText(holesNames: Seq[(String, HoleDef)]): String = {
    holesNames.filter(_._2.isGrammarDefined).map { case (id, h) =>
      s"$id,${h.grammar}"
    }.mkString(";")
  }
  
  /**
   * Returns a Python code representing the given set of test cases. The returned text is a valid
   * value for --test_cases command line argument of PySV framework. 
   */
  def tests2py[T](tests: List[Test[List[T], T]], inputNames: Seq[String], outputNames: Seq[String]): String = {
    val sInVars = "[" + inputNames.map("'"+_+"'").mkString(",") + "]"
    val sOutVars = "[" + outputNames.map("'"+_+"'").mkString(",") + "]"
    "([" + tests.map(PythonUtils.pyTest(_)).mkString(",  ") + s"],    ${sInVars}, ${sOutVars})"
  }
}



/**
 * Stores data returned by the SMT solver.
 */
class SolverResult(val decision: String,
                   val model: Option[Map[String, String]] = None,
                   val unsatCore: Option[Seq[String]] = None,
                   val assignments: Option[Map[String, String]] = None,
                   val holesContent: Option[Map[String, String]] = None) {
  def isSat = decision == SolverResult.SAT
  def isUnsat = decision == SolverResult.UNSAT
  def isUnknown = decision == SolverResult.UNKNOWN
  def isTimeout = decision == SolverResult.TIMEOUT
  def isInvalid = decision == SolverResult.INVALID
  def isDefined = isSat || isUnsat
}


object SolverResult {
  val SAT = "sat"
  val UNSAT = "unsat"
  val UNKNOWN = "unknown"
  val INVALID = "invalid"
  val TIMEOUT = "timeout"
  val ERROR = "error"
  val allDecisions = List(SAT, UNSAT, UNKNOWN, INVALID, TIMEOUT, ERROR)
  def apply(decision: String) = new SolverResult(decision)
  
  /**
   * Creates SolverResult from the PySV output. Output always starts with the decision (sat/unsat/unknown).
   * @param s output returned by PySV.
   * @param solverOutSeq order and types of data returned by PySV (may be customized).
   */
  def apply(s: String, solverOutSeq: List[String] = List("decision", "model")): SolverResult = {
    assert(solverOutSeq.head == "decision")
    var lines = s.split("\n").toList
    var decision = ""
    var model: Option[Map[String, String]] = None
    var unsatCore: Option[Seq[String]] = None
    var assignments: Option[Map[String, String]] = None
    var holesContent: Option[Map[String, String]] = None
    def loadDecision(lines: List[String]): (String, List[String]) = {
      return (lines.head, lines.tail)
    }
    def loadModel(decision: String, lines: List[String]): (Option[Map[String, String]], List[String]) = {
      if (decision != SolverResult.SAT)
        (None, lines.tail)
      else {
        val model = PythonUtils.pyDictToScalaMap(lines.head)
        (Some(model), lines.tail)
      }
    }
    def loadUnsatCore(decision: String, lines: List[String]): (Option[Seq[String]], List[String]) = {
      ???
    }
    def loadAssignments(decision: String, lines: List[String]): (Option[Map[String, String]], List[String]) = {
      if (decision != SolverResult.SAT)
        (None, lines.tail)
      else {
        val assigns = PythonUtils.pyDictToScalaMap(lines.head)
        (Some(assigns), lines.tail)
      }
    }
    def loadHolesContent(decision: String, lines: List[String]): (Option[Map[String, String]], List[String]) = {
      if (decision != SolverResult.SAT)
        (None, lines.tail)
      else {
        val holesContent = PythonUtils.pyDictToScalaMap(lines.head)
        (Some(holesContent), lines.tail)
      }
    }
    // println("Lines from PySV: " + lines.mkString("\n"))
    solverOutSeq.foreach { x =>
      x match {
        case "decision" => val (v, l) = loadDecision(lines); decision = v; lines = l
        case "model" => val (v, l) = loadModel(decision, lines); model = v; lines = l
        case "unsat_core" => val (v, l) = loadUnsatCore(decision, lines); unsatCore = v; lines = l
        case "assignments" => val (v, l) = loadAssignments(decision, lines); assignments = v; lines = l
        case "holes_content" => val (v, l) = loadHolesContent(decision, lines); holesContent = v; lines = l
        case _ => throw new Exception(s"Unknown key '$x' in the specification of the output data!")
      }
    }
    new SolverResult(decision, model, unsatCore, assignments, holesContent)
  }
}




object PythonUtils {
  /**
   * Returns a Python tuple representing the given test case.
   */
  def pyTest[T](test: Test[List[T], T]): String = {
        val inContent = test.input.mkString(",")
        val outContent = test.output
        "([" + inContent + "], [" + outContent + "])"
  }
  
  /**
   * Converts string representation of a Python dictionary into a Scala Map instance.
   */
  def pyDictToScalaMap(code: String): Map[String, String] = {
    if (code.equals("{}")) Map() // the case of "{}"
    else {
      val pairs = code.substring(1, code.size-1).split(",")
      pairs.map { p => val w = p.split(":")
        (w(0).replace("'", "").trim(), w(1).replace("'", "").trim())
      }.toMap
    }
  }
}