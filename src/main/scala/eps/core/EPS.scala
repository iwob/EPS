package eps.core

import java.nio.file.{Paths, Files}
import fuel.func.RunExperiment
import fuel.util.Rng
import fuel.func.ParallelEval
import fuel.util.TRandom
import fuel.moves.Moves
import fuel.func.SimpleBreeder
import fuel.util.CollectorFile
import fuel.func.SequentialEval
import fuel.util.Collector
import fuel.core.StatePop
import swim.tree.Op
import swim.tree.GPMoves
import swim.tree.SimpleGP
import eps.utils.OptionsEPS
import eps.smt.Holes
import eps.smt.SmtlibUtils
import eps.smt.RunnerPySV


/**
 * Main class handling the evolution.
 */
class EPS(problem: ProblemDefinition,
          moves: Moves[Op],
          eval: Op => FitnessEPS,
          dataColl: DataCollector,
          stop: (Op, FitnessEPS) => Boolean = ((s: Op, e: FitnessEPS) => false))
         (implicit env: OptionsEPS,
          coll: Collector,
          rng: TRandom,
          ordering: Ordering[FitnessEPS])
         extends SimpleGP[Seq[Any], Any, FitnessEPS](moves, eval, stop)(env.options, coll, rng, ordering) {
  implicit val options = env.options
  val isSilent = env.getBool(OptionsEPS.silent)
  val optFillHoles = env.getBool(OptionsEPS.fillHoles)
  val evaluation = if (env.options('parEval, true)) ParallelEval(eval) else SequentialEval(eval)
  
  def printPop(pop: StatePop[(Op,FitnessEPS)]): StatePop[(Op,FitnessEPS)] = {
    if (!isSilent) {
      println("POPULATION AFTER ITERATION:")
      pop.foreach(println(_))
    }
    // Printing memory usage
    /*val kb = 1024
    val unit = kb
    val runtime = Runtime.getRuntime
    println("** Used Memory:  " + ((runtime.totalMemory - runtime.freeMemory) / unit))*/
    pop
  }
  
  def collectData(pop: StatePop[(Op,FitnessEPS)]): StatePop[(Op,FitnessEPS)] = {
    dataColl.collectData(pop)
    pop
  }
  
  /**
   * If the given Op contains a hole and optimizing solver ended with SAT, then the hole will
   * be replaced with the appropriate content taken from the model returned by the solver.
   * This model corresponds to a program code which optimizes the number of passed test cases.
   */
  def fillHolesInOp(op: Op, fit: FitnessEPS): Op = {
    if (fit.solverRes.isDefined &&
        fit.solverRes.get.isSat) {
      val (renamedOp, holeNames) = Holes.renameHoles(op, problem.holesDefs)
      val filledOp = Holes.fillHoles(renamedOp, fit.solverRes.get, holeNames, Some(problem.grammar), Some(problem.inputVars))
      if (!isSilent) {
        println("Op before fillHoles: " + op)
        println("Op after fillHoles: " + filledOp)
      }
      filledOp
    }
    else op // no holes to be filled
  }
  
  /**
   * Depending on the experiment's options, either fills holes in the every solution in the population with the content extracted from the model
   * ("Lamarckian" variant, EPS-L), or leaves the solutions with holes ("Baldwinian" variant, EPS-B).
   */
  def fillHoles(pop: StatePop[(Op, FitnessEPS)]): StatePop[(Op, FitnessEPS)] = {
    if (optFillHoles) StatePop(pop.map { case (op, e) => (fillHolesInOp(op, e), e) })
    else pop
  }
  
  override def initialize = super.initialize
  override def evaluate = evaluation andThen fillHoles andThen report andThen collectData
  override def iter = SimpleBreeder(selection, moves: _*) andThen evaluate andThen printPop
}


object EPS {
  def apply(problem: ProblemDefinition, dataColl: DataCollector)
           (implicit env: OptionsEPS,
            coll: Collector,
            rng: TRandom,
            ordering: Ordering[FitnessEPS]): EPS = {
    implicit val options = env.options
    def isFeasible = (op: Op) => op.height < 13 && op.size < 70
    def stopCondition = (s:Op, e:FitnessEPS) => e.value == problem.tests.size
    val isSilent = env.getBool(OptionsEPS.silent)
    val moves = new GPMoves(problem.grammar, isFeasible)
    val pysv = RunnerPySV(problem)
    
    /**
     * Evaluates a GP tree representing SMT-LIB 2.0 expression. If any holes are present,
     * SMT solver will be used to find their content. Otherwise, program will be evaluated
     * by running it on a set of test cases.
     */
    def eval(op: Op): FitnessEPS = {
      try {
        def opContainsHoles(op: Op) = problem.holesDefs.exists { h => op.contains(h.name) }
        if (!isSilent)
          println("EVALUATING: " + op)
        val t0 = System.currentTimeMillis()
        val fit = if (opContainsHoles(op)) {
            dataColl.evalsSolver += 1
            Evaluation.evalHoles(op, problem, pysv)
          }
          else {
            dataColl.evalsNormal += 1
            Evaluation.evalNormally(op, problem)
          }
        val duration = (System.currentTimeMillis() - t0)
        FitnessEPS(fit.value, fit.solverRes, Some(duration))
      } catch {
        case e: Throwable => e.printStackTrace(); FitnessEPS(0, None)
      }
    }
    
    new EPS(problem, moves, eval, dataColl, stopCondition)
  }
}





object ExperimentEPS {
  def apply(env: OptionsEPS): ExperimentEPS = {
    val rng = Rng(env.options)
    val problem = ProblemDefinition(env)(rng)
    new ExperimentEPS(problem)(env, rng)
  }
}


class ExperimentEPS(val problem: ProblemDefinition)
                   (implicit val env: OptionsEPS,
                    implicit val rng: TRandom) {
  implicit val options = env.options
  implicit val coll = new CollectorFile(options)
  implicit val ordering = Ordering[Int].reverse
  val dataColl = DataCollector(env)
  val eps = EPS(problem, dataColl)
  
  def checkPysvPath() {
    val path = env.getString(OptionsEPS.pathToPySV)
    if (problem.holesDefs.size > 0 && !Files.exists(Paths.get(path)))
      throw new Exception(s"The main.py file of the pysv framework was not found at ${path}! Check your --eps.pathToPySV option.")
  }
  
  def run(): (Op, FitnessEPS) = {
    try {
      checkPysvPath()
      val finalPop = RunExperiment(eps)
      val bestOfRun = eps.bsf.bestSoFar.get
      printRunConclusion(finalPop.get, bestOfRun)
      saveEvolutionLogs(bestOfRun)
      bestOfRun
    } catch {
      case e: Throwable =>
        coll.set("eps.exception.message", e.getMessage.replace("\n", " "))
        coll.set("eps.exception.stacktrace", e.getStackTrace.mkString("  "))
        coll.saveSnapshot("eps.exception")
        throw e
    }
  }
  
  def confirmOptimalityOnTests(op: Op): Boolean = {
    val e = Evaluation.evalNormally(op, problem)
    e.value == problem.tests.size
  }
  
  def printRunConclusion(finalPop: StatePop[(Op, FitnessEPS)], bestOfRun: (Op, FitnessEPS)) {
    if (!env.getBool(OptionsEPS.silent)) {
      printFinalPop(finalPop)
      printBestOfRun(bestOfRun)
      printStatistics()
    }
  }
  
  def printFinalPop(finalPop: StatePop[(Op, FitnessEPS)]) {
    println("\nFINAL POPULATION:")
    println(finalPop.map{ case (s, e) => (SmtlibUtils.opToSmtlib(s), " fit="+e, " height="+s.height)}.mkString("\n"))
  }
  
  def printBestOfRun(bestOfRun: (Op, FitnessEPS)) {
    val (op, fit) = bestOfRun
    println("\nBEST OF RUN:")
    println(bestOfRun)
    if (fit.solverRes.isDefined) {
      val res = fit.solverRes.get
      println("Decision: " + fit.solverRes.get.decision)
      if (res.isSat) {
        val (opSubst, holesNamesTypes) = Holes.renameHoles(op, problem.holesDefs)
        val optSol = Holes.fillHoles(opSubst, fit.solverRes.get, holesNamesTypes, Some(problem.grammar), Some(problem.inputVars))
        println("\n\nFINAL SOLUTION")
        println("Before substitution:".padTo(23, ' ') + opSubst)
        println("After substitution:".padTo(23, ' ') + optSol)
        runSolutionOnTests(optSol)
      }
    }
    else {
      println("Solution does not contain holes, so no substitution is needed.")
      runSolutionOnTests(op)
    }
  }
  
  def runSolutionOnTests(op: Op) {
    println("")
    println("")
    println("TESTING FINAL SOLUTION:")
    problem.tests.foreach{ test =>
      try {
        val res = problem.domain.semantics(test.input.asInstanceOf[Seq[Any]])(op)
        print(test.input.mkString("(", ", ", ")"))
        print(",  expected: " + test.output + ",  obtained: " + res)
        print("\n")
      } catch { case _:Throwable =>
        print(test.input.mkString("(", ", ", ")"))
        print(",  expected: " + test.output + ",  obtained: " + "ERROR")
        print("\n")
      }
    }
  }
  
  def printStatistics() {
    println("")
    println("")
    println("STATISTICS:")
    println("Total evaluated: " + dataColl.evalsTotal)
    println("Evaluated with solver: " + dataColl.evalsSolver)
    println("- sat: " + dataColl.getNumSat)
    println("- unsat: " + dataColl.getNumUnsat)
    println("- unknown: " + dataColl.getNumUnknown)
    println("- invalid: " + dataColl.getNumInvalid)
    println("- timeout: " + dataColl.getNumTimeout)
    println("- error: " + dataColl.getNumError)
    println("Evaluated with solver ratio: " + round(dataColl.evalsSolver.asInstanceOf[Double] / dataColl.evalsTotal))
    println("")
    val fitStats = dataColl.fitnessStatsIterations()
    println("result.stats.avgFitness: " + fitStats.map(_._1).map{ x => round(x) }.mkString(", "))
    println("result.stats.minFitness: " + fitStats.map(_._2).mkString(", "))
    println("result.stats.maxFitness: " + fitStats.map(_._3).mkString(", "))
    println("result.stats.stdFitness: " + fitStats.map{ x => round(x._4) }.mkString(", "))
    println("Avg size: " + dataColl.avgIterations(IterationData.SIZE).map{ x => round(x) }.mkString(", "))
    println("Avg height: " + dataColl.avgIterations(IterationData.HEIGHT).map{ x => round(x) }.mkString(", "))
    println("Avg evaluation time [ms]: " + dataColl.avgIterations(IterationData.TIME).map{ x => round(x,1) }.mkString(", "))
    println("Avg normal evaluation time [ms]: " + dataColl.avgIterations(IterationData.EVAL_NORMAL_TIME).map{ x => round(x,1) }.mkString(", "))
    println("Avg solver evaluation time [ms]: " + dataColl.avgIterations(IterationData.EVAL_HOLES_TIME).map{ x => round(x,1) }.mkString(", "))
  }
  
  def saveEvolutionLogs(bestOfRun: (Op, FitnessEPS)) {
    if (env.getBool(OptionsEPS.saveLogs)) {
      val (bestOp, bestFit) = bestOfRun
      val bestOpFilled = Holes.fillHolesInOp(bestOp, bestFit, problem)
      val fitNormalEval = if (!bestFit.wasSolverUsed) bestFit
                          else Evaluation.evalNormally(bestOpFilled, problem)
      
      // Best found solution and it's fitness are saved under standard FUEL keys:
      // "result.best", "result.best.eval".
      
      // Saving some other best of run information.
      coll.set("result.best.holesFilled", bestOpFilled)
      coll.set("result.best.isOptimal", if (bestFit.value == problem.tests.size) 1 else 0)
      coll.set("result.best.fitness", bestFit.value)
      coll.set("result.best.evalNormally", fitNormalEval.value)
      
      // Saving some additional run stats.
      coll.set("result.stats.totalEvaluated", dataColl.evalsTotal)
      coll.set("result.stats.evaluatedSolver", dataColl.evalsSolver)
      coll.set("result.stats.evaluatedSolverSat", dataColl.getNumSat)
      coll.set("result.stats.evaluatedSolverUnsat", dataColl.getNumUnsat)
      coll.set("result.stats.evaluatedSolverUnknown", dataColl.getNumUnknown)
      coll.set("result.stats.evaluatedSolverInvalid", dataColl.getNumInvalid)
      coll.set("result.stats.evaluatedSolverError", dataColl.getNumError)
      coll.set("result.stats.evaluatedSolverTimeout", dataColl.getNumTimeout)
      coll.set("result.stats.evaluatedSolverRatio", round(dataColl.evalsSolverRatio))
      val fitStats = dataColl.fitnessStatsIterations()
      coll.set("result.stats.avgFitness", fitStats.map(_._1).map{ x => round(x) }.mkString(", "))
      coll.set("result.stats.minFitness", fitStats.map(_._2).mkString(", "))
      coll.set("result.stats.maxFitness", fitStats.map(_._3).mkString(", "))
      coll.set("result.stats.stdFitness", fitStats.map{ x => round(x._4) }.mkString(", "))
      coll.set("result.stats.avgSize", dataColl.avgIterations(IterationData.SIZE).map{ x => round(x) }.mkString(", "))
      coll.set("result.stats.avgHeight", dataColl.avgIterations(IterationData.HEIGHT).map{ x => round(x) }.mkString(", "))
      coll.set("result.stats.avgEvalTime", dataColl.avgIterations(IterationData.TIME).map{ x => round(x,1) }.mkString(", "))
      coll.set("result.stats.avgNormalEvalTime", dataColl.avgIterations(IterationData.EVAL_NORMAL_TIME).map{ x => round(x,1) }.mkString(", "))
      coll.set("result.stats.avgSolverEvalTime", dataColl.avgIterations(IterationData.EVAL_HOLES_TIME).map{ x => round(x,1) }.mkString(", "))
      
      coll.saveSnapshot("eps")
    }
  }
  
  def round(x: Double, d: Int = 2): String = s"%.${d}f".formatLocal(java.util.Locale.US, x)
}