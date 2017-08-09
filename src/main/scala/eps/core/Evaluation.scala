package eps.core

import eps.smt.SolverResult
import eps.utils.EPSUtils
import eps.smt.Holes
import eps.utils.OptionsEPS
import swim.tree.Op
import eps.smt.RunnerPySV
import eps.smt.HoleDef


class FitnessEPS(val value: Int, val solverRes: Option[SolverResult] = None, val evalTime: Option[Double] = None) extends Ordered[FitnessEPS] {
  override def toString: String = value.toString
  def wasSolverUsed: Boolean = solverRes.isDefined
  def compare(that: FitnessEPS): Int = that.value compare this.value
}

object FitnessEPS {
  def apply(value: Int, solverRes: Option[SolverResult] = None, evalTime: Option[Double] = None) = new FitnessEPS(value, solverRes, evalTime)
  def apply(tuple: (Int, Option[SolverResult])): FitnessEPS = new FitnessEPS(tuple._1, tuple._2)
}




object Evaluation {
  /**
   * Evaluates a tree without holes by running it on all test cases. Fitness is the number of passed test cases.
   */
  def evalNormally(op: Op, problem: ProblemDefinition, doPrint: Boolean = false): FitnessEPS = {
    val results = problem.tests.map{ test =>
      try {
        val out = problem.domain.semantics(test.input)(op)
        if (doPrint) {
          print(test.input.mkString("(", ", ", ")"))
          println(",  expected: " + test.output + ",  obtained: " + out)
        }
        if (out == test.output) 1 else 0
      } catch {
        case e: Throwable => // e.g. if division by 0 error.
          if (doPrint) {
            print(test.input.mkString("(", ", ", ")"))
            println(",  expected: " + test.output + ",  obtained: ERROR (" + e.getMessage + ")")
          }
          0
      }
    }
    FitnessEPS(results.sum, None)
  }
  
  /**
   * Checks, if the given op is valid. For example it is checked, if there is a division by 0 constant.
   * Invalid Ops will automatically get the lowest fitness.
   */
  def isOpValid(op: Op): Boolean = {
    op match {
      case Op(_, 'div, _, Op(_, 0)) => false
      case Op(_, '/, _, Op(_, 0)) => false
      case _ => op.args.forall(isOpValid(_))
    }
  }
  
  /**
   * Evaluates a solution with holes. Depending on the experiment's options tries
   * to maximize the number of passed test cases or not.
   */
  def evalHoles(op: Op, problem: ProblemDefinition, pysv: RunnerPySV): FitnessEPS = {
    try {
      if (!isOpValid(op)) FitnessEPS(0, Some(SolverResult(SolverResult.INVALID)))
      else {
        val optEnabled = problem.env.getBool(OptionsEPS.optimizeNumPassedTestCases)
        val (newOp, holesNames) = Holes.renameHoles(op, problem.holesDefs)
        if (optEnabled)
          evalOpt(newOp, holesNames, problem, pysv)
        else
          evalStandardSynthesis(newOp, holesNames, problem, pysv)
      }
    } catch {
      case e: Throwable => e.printStackTrace(); FitnessEPS(0, Some(SolverResult(SolverResult.ERROR)))
    }
  }
  
  
  /**
   * Checks, if there exists a content for holes which will make a solution optimal
   * with respect to the test cases. If such a solution was not found, then fitness
   * will be 0. If such a solution was found, then fitness will be equal to the number
   * of test cases. From the evolutionary perspective this is not a good evaluation
   * function, because it provides no information to guide the search.
   */
  def evalStandardSynthesis(op: Op,
                            holesNames: Seq[(String, HoleDef)],
                            problem: ProblemDefinition,
                            pysv: RunnerPySV): FitnessEPS = {
    val res = pysv.runSynthesis(op, holesNames)
    res.decision match {
      case SolverResult.SAT => FitnessEPS(problem.tests.size, Some(res))
      case SolverResult.UNSAT | SolverResult.UNKNOWN | SolverResult.TIMEOUT => FitnessEPS(0, Some(res))
      case _ => throw new Exception(s"Solver docision '${res.decision}': was not recognized!")
    }
  }
  
  
  /**
   * Evaluates a solution with holes by trying to maximize the number of passed test cases.
   */
  def evalOpt(op: Op,
              holesNames: Seq[(String, HoleDef)],
              problem: ProblemDefinition,
              pysv: RunnerPySV): FitnessEPS = {
    val optMode = problem.env.getString(OptionsEPS.optimizationMode)
    if (optMode == "bisecting")
      evalOptBisecting(op, holesNames, problem, pysv)
    else
      evalOptSolver(op, holesNames, problem, pysv)
  }
  
  def evalOptBisecting(op: Op,
                       holesNames: Seq[(String, HoleDef)],
                       problem: ProblemDefinition,
                       pysv: RunnerPySV): FitnessEPS = {
    def bisecting(interval: (Int,Int), prevRes: Option[SolverResult]): Option[SolverResult] = {
      if (interval._1 > interval._2) prevRes
      else {
        val b = (interval._1 + interval._2) / 2
        val res = pysv.runSynthesisOpt(op, holesNames, Some(b))
        res.decision match {
          case SolverResult.SAT => bisecting((b+1, interval._2), Some(res))
          case SolverResult.UNSAT => bisecting((interval._1, b-1), prevRes)
          case _ => None
        }
      }
    }
    val res = bisecting((1, problem.tests.size), None)
    if (res.isDefined)
      solverResToFitness(res.get)
    else
      FitnessEPS(0, Some(SolverResult(SolverResult.UNKNOWN)))
  }
  
  def evalOptSolver(op: Op,
                    holesNames: Seq[(String, HoleDef)],
                    problem: ProblemDefinition,
                    pysv: RunnerPySV): FitnessEPS = {
    val res = pysv.runSynthesisOpt(op, holesNames)
    solverResToFitness(res)
  }
  
  def solverResToFitness(res: SolverResult): FitnessEPS = {
    if (res.isSat) {
      val numPassTests = EPSUtils.strToInstance(res.model.get.get("fitness").get, "Int")
      FitnessEPS(numPassTests.asInstanceOf[Int], Some(res))
    }
    else if (res.isUnknown || res.isUnsat || res.isUnknown || res.isTimeout) FitnessEPS(0, Some(res))
    else throw new Exception(s"Unhandled solver's decision: ${res.decision}!")
  }
}