package eps.core

import scala.collection.mutable.MutableList
import fuel.core.StatePop
import swim.tree.Op
import eps.smt.SolverResult
import eps.utils.OptionsEPS


object DataCollector {
  def apply(env: OptionsEPS): DataCollector = {
    val logsResolution = env.getInt(OptionsEPS.logsResolution)
    new DataCollector(logsResolution)
  }
}


class DataCollector(logsResolution: Int = 1) {
  var evalsNormal = 0
  var evalsSolver = 0
  def evalsTotal = evalsNormal + evalsSolver
  def evalsSolverRatio: Double = evalsSolver.asInstanceOf[Double] / evalsTotal.asInstanceOf[Double]
  def getNumSat: Int = iterations.map(_.numSat).sum
  def getNumUnsat: Int = iterations.map(_.numUnsat).sum
  def getNumUnknown: Int = iterations.map(_.numUnknown).sum
  def getNumInvalid: Int = iterations.map(_.numInvalid).sum
  def getNumTimeout: Int = iterations.map(_.numTimeout).sum
  def getNumError: Int = iterations.map(_.numError).sum
  
  val iterations = MutableList[IterationData]()
  var iterNum = 0
  def collectData(pop: StatePop[(Op,FitnessEPS)]) {
    if (iterNum % logsResolution == 0)
      iterations += new IterationData(pop)
    iterNum += 1
  }
  def avgIterations(symb: Symbol): Seq[Double] = iterations.map(_.avg(symb))
  def fitnessStatsIterations(): Seq[(Double, Double, Double, Double)] =
    iterations.map(_.seqStats(IterationData.FITNESS))
}

class IterationData(pop: StatePop[(Op,FitnessEPS)]) {
  def fitnesses = pop.map{ case (op, f) => f.value.toDouble }
  def sizes = pop.map{ case (op, f) => op.size.toDouble }
  def heights = pop.map{ case (op, f) => op.height.toDouble }
  def times = pop.map{ case (op, f) => f.evalTime.get }
  def evalsHoleTimes: Seq[Double] = {
    val smtEvaled = pop.filter{ case (op, f) => f.wasSolverUsed && !f.solverRes.get.isInvalid }.map(_._2)
    smtEvaled.map{ f => f.evalTime.getOrElse(-1.0) }
  }
  def evalsNormalTimes = {
    pop.filter(!_._2.wasSolverUsed).map{ case (op, f) => f.evalTime.getOrElse(-1.0) }
  }
  val decisionsMap = SolverResult.allDecisions.map { dec:String =>
    (dec, pop.filter{ case (op, f) => f.wasSolverUsed && f.solverRes.get.decision == dec }.size)
  }.toMap
  def numSat = decisionsMap(SolverResult.SAT)
  def numUnsat = decisionsMap(SolverResult.UNSAT)
  def numUnknown = decisionsMap(SolverResult.UNKNOWN)
  def numInvalid = decisionsMap(SolverResult.INVALID)
  def numTimeout = decisionsMap(SolverResult.TIMEOUT)
  def numError = decisionsMap(SolverResult.ERROR)
  
  def getSeq(symb: Symbol): Seq[Double] = {
    symb match {
      case IterationData.FITNESS => fitnesses
      case IterationData.SIZE => sizes
      case IterationData.HEIGHT => heights
      case IterationData.TIME => times
      case IterationData.EVAL_HOLES_TIME => evalsHoleTimes
      case IterationData.EVAL_NORMAL_TIME => evalsNormalTimes
    }
  }
  def avg(symb: Symbol): Double = {
    val seq = getSeq(symb)
    if (seq.isEmpty) -1
    else seq.sum / seq.size
  }
  /**
   * Returns avg, min, max and stddev.
   */
  def seqStats(symb: Symbol): (Double, Double, Double, Double) = {
    val seq = getSeq(symb)
    if (seq.isEmpty) (-1, -1, -1, -1)
    else {
      val mean = seq.sum / seq.size
      (mean, seq.min, seq.max, calcStddev(seq, mean))
    }
  }
  def calcStddev(seq: Seq[Double], mean: Double): Double = {
    Math.sqrt(seq.map{ x => (x-mean) * (x-mean) }.sum / seq.size)
  }
}

object IterationData {
  val FITNESS = 'fitness
  val SIZE = 'size
  val HEIGHT = 'height
  val TIME = 'time
  val EVAL_HOLES_TIME = 'eval_holes_time
  val EVAL_NORMAL_TIME = 'eval_normal_time
}