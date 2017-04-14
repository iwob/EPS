package eps.app

import fuel.util.Options
import eps.core.ProblemDefinition
import eps.core.ExperimentEPS
import eps.utils.OptionsEPS
import fuel.util.Rng


object Main {
  def getPrespecifiedOptions(): OptionsEPS = {
    val sopts =
      """
      --deleteOutputFile true
      --operatorProbs 0.5,0.5
      --tournamentSize 7
      --parEval false
      --initMaxTreeDepth 4
      --maxSubtreeDepth 4
      --populationSize 100
      --maxGenerations 3
      --eps.logic QF_NIA
      --eps.pathTests data/int/koza1-p.csv
      --eps.enableHoles true
      --eps.useInputVarsAsTerminals true
      --eps.useConstantProvider false
      --eps.holesVars varInt:Int:x
      --eps.holesConsts constInt:Int
      --eps.fillHoles false
      --eps.silent false
      --eps.saveLogs true
      """
    println("Warning: using prespecified options.")
    OptionsEPS(Options(sopts))
  }
  
  def run(env: OptionsEPS) {
    val exp = ExperimentEPS(env)
    exp.run()
  }
  
  def printVersion() {
    println("EPS 1.0")
  }
  
  def main(args: Array[String]) {
    val DEBUG_RUN = false
    if (args.contains("-h") || args.contains("--help"))
      OptionsEPS.printOptions()
    else if (args.contains("-v") || args.contains("--version"))
      printVersion()
    else if (args.size == 0 || DEBUG_RUN)
      // Options preset manually.
      run(getPrespecifiedOptions())
    else
      // Options from a command line.
      run(OptionsEPS(Options(args)))
  }
}