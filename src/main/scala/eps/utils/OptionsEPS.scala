package eps.utils

import fuel.util.Options


class OptionsEPS(val options: Options) {
  def getRawOption(opt: OptionInfo): Option[String] = options.getOption(opt.name)
  def validateValue(value: Option[String], opt: OptionInfo): Option[String] = {
    if (value.isEmpty) value
    else if (opt.choice.isEmpty || opt.choice.contains(value.get)) value
    else throwIncorrectChoiceException(opt, value.get)
  }
  def getOption(opt: OptionInfo): Option[String] = {
    val value = validateValue(getRawOption(opt), opt)
    if (value.isDefined) value
    else if (opt.default.isDefined) opt.default
    else None
  }
  def getString(opt: OptionInfo): String = {
    if (getOption(opt).isDefined) getOption(opt).get else throwMissingParamException(opt)
  }
  def getBool(opt: OptionInfo): Boolean = {
    if (getOption(opt).isDefined) getOption(opt).get.toBoolean else throwMissingParamException(opt)
  }
  def getInt(opt: OptionInfo): Int = {
    if (getOption(opt).isDefined) getOption(opt).get.toInt else throwMissingParamException(opt)
  }
  def getDouble(opt: OptionInfo): Double = {
    if (getOption(opt).isDefined) getOption(opt).get.toDouble else throwMissingParamException(opt)
  }
  def throwMissingParamException(opt: OptionInfo) = throw new Exception(s"Parameter '${opt.name}' not found!")
  def throwIncorrectChoiceException(opt: OptionInfo, value: String) = throw new Exception(s"Parameter '${opt.name}' has incorrect value: '$value'! Possible values: ${opt.choice.mkString("'", "', '","'")}.")
}


object OptionsEPS {
  val logic = new OptionInfo("eps.logic", "String",
    desc="Logic of evolved programs.")
  val pathTests = new OptionInfo("eps.pathTests", "String",
    desc="Path to a CSV file containing test cases.")
  val enableHoles = new OptionInfo("eps.enableHoles", "Bool", default=Some("true"),
    desc="Determines, if holes will be used in this evolution run.")
  val fillHoles = new OptionInfo("eps.fillHoles", "Bool", default=Some("false"),
    desc="If set to true, then content of the hole will be permanently inserted to a solution in the place of the hole.")
  val holesConsts = new OptionInfo("eps.holesConsts", "List[String]", default=Some(""),
    desc="List of names Ni and types Ti (format: N1:T1;N2:T2,...) of all holes treated as terminals in the programs to be synthesized (e.g. constInt:Int).")
  val holesVars = new OptionInfo("eps.holesVars", "List[String]", default=Some(""),
    desc="List of names and types (format: N1:T1:v1,v2,...,vn;N2:T2:v1,...) of all holes which may be filled with variables.")
  val optimizeNumPassedTestCases = new OptionInfo("eps.optimizeNumPassedTestCases", "Bool", default=Some("true"),
    desc="If set to true, then holes content will be optimized with respect to the number of passed test cases. (recommended: true)")
  val optimizationMode = new OptionInfo("eps.optimizationMode", "String", default=Some("solver"), choice=Set("bisecting", "solver"),
    desc="Specifies method of optimization. If 'solver', then optimization will be done by Z3 inbuilt optimization functionality. If 'bisecting', then optimization is performed by several calls to solver with different limit on the number of passed test cases.")
  val useConstantProvider = new OptionInfo("eps.useConstantProvider", "Bool", default=Some("true"),
    desc="Specifies, if constant provider should be added to the instruction set as terminal (ephemeral random constant).")
  val useInputVarsAsTerminals = new OptionInfo("eps.useInputVarsAsTerminals", "Bool", default=Some("true"),
    desc="Specifies, if input variables will be added to the instruction set as terminals.")
  val safetyConditions = new OptionInfo("eps.safetyConditions", "Bool", default=Some("true"),
    desc="If set to true, then for every solution will be generated safety conditions protecting against the division by 0. If you do not have division in the instruction set, then you can switch off this option for a little increase in performance.")
  val solverTimeout = new OptionInfo("eps.solverTimeout", "Int", default=Some("1500"),
    desc="Time in miliseconds after which solver will abort search for a solution.")
  val pathToPySV = new OptionInfo("eps.pathToPySV", "String", default=Some("../pysv/main.py"),
    desc="Path to a main.py file in the PySV (Python Synsthesis & Verification) project.")
  val silent = new OptionInfo("eps.silent", "Bool", default=Some("false"),
  desc="Enables or disables additional comments of the EPS module.")
  val saveLogs = new OptionInfo("eps.saveLogs", "Bool", default=Some("false"),
    desc="Enables or disables saving EPS evolution logs.")
  val logsResolution = new OptionInfo("eps.logsResolution", "Int", default=Some("1"),
    desc="Specifies, how often evolution logs should be collected. '1' means that data of every iteration is stored, '2' that data of every second iteration is stored etc.")
  
  val allOptions = Seq(logic, pathTests, enableHoles, fillHoles, holesConsts, holesVars, optimizeNumPassedTestCases,
      optimizationMode, useConstantProvider, useInputVarsAsTerminals, safetyConditions, solverTimeout,
      pathToPySV, silent, saveLogs, logsResolution)
  def apply(options: Options) = new OptionsEPS(options)
  def printOptions() {
    println("Parameters:")
    allOptions.sortBy(_.name).foreach { optInfo => print(optInfo) }
  }
}


/**
 * Stores all information related to a certain option.
 */
class OptionInfo(val name: String, val tpe: String = "", val desc: String = "", val default: Option[String] = None,
                 val choice: Set[String] = Set()) {
  override def toString(): String = {
    def textDefault(s: Option[String]) = if (s.isDefined) " (default: " + s.get + ")" else ""
    name.padTo(40, ' ') + tpe.padTo(20, ' ') + desc + textDefault(default) + "\n"
  }
}