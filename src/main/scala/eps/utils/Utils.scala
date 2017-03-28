package eps.utils

import eps.core.VarDef
import swim.Test
import scala.io.Source

object EPSUtils {
  /**
   * Changes String to instance of a given type. If an unknown type name is given or type name is omitted, then
   * type is automatically inferred from the String.
   */
  def strToInstance(s: String, tpe: String = ""): Any = {
    def isBoolean(s: String): Boolean = if (s == "true" || s == "false") true else false
    def isInt(s: String): Boolean = try { val x = s.toInt; true } catch { case _:Throwable => false }
    def isDouble(s: String): Boolean = try { val x = s.toDouble; true } catch { case _:Throwable => false }
    def isString(s: String): Boolean = if (s.head == '\"' && s.last == '\"') true else false
    val supportedTypes = Set("Bool", "Int", "Double", "String", "Symbol")
    if (supportedTypes.contains(tpe))
      tpe match {
        case "Bool" => s.toBoolean
        case "Int" => s.toInt
        case "Double" => s.toBoolean
        case "String" => s.substring(1, s.size-1)
        case "Symbol" => Symbol(s)
      }
    else
       // Infer type automatically.
      if (isBoolean(s)) s.toBoolean
      else if (isInt(s)) s.toInt
      else if (isDouble(s)) s.toDouble
      else if (isString(s)) s.substring(1, s.size-1)
      else Symbol(s)
  }
  
  /**
   * Returns a default value from a domain of the provided type.
   */
  def defaultValue(tpe: String): Any = {
    tpe match {
      case "Bool" => false
      case "Int" => 0
      case "Double" => 0.0
      case "String" => ""
      case _ => throw new Exception(s"Tried to get default value for the unknown type: ${tpe}!")
    }
  }
  
  /**
   * Load tests from csv file with values delimited by ';'. Comments are marked with # and are ignored.
   * First non-ignored row of the file must contain variable names and their types. Example file may look
   * like this:
   * <pre>
   * {@code
   * # This is an example csv file with test cases.
   * x:Int; res:Int
   * 0; 1
   * 1; 2
   * }</pre>
   */
  def loadTestsFile(path: String, varNamesInHeader: Boolean = true): (List[Test[List[Any],Any]], List[VarDef], List[VarDef]) = {
    def readLines(path: String) = Source.fromFile(path).getLines()
    def readVarDefinitions(words: List[String]): List[VarDef] = {
      words.map { w =>
        val splitted = w.trim().split(":")
        new VarDef(Symbol(splitted(0)), splitted(1))
      }
    }
    val content = readLines(path).filter{ s => s.nonEmpty && s.trim().head != '#' }.map(_.split(';').toList.map(_.trim())).toList
    assert(content.nonEmpty, "It appears there are no valid tests to read from " + path)
    val numFields = content(0).length
    assert(content.forall(_.length == content(0).length), "All tests have to be of the same arity.")
    val varDefs = readVarDefinitions(content.head)
    val tests = content.tail.map { words: List[String] =>
      val input: List[Any] = words.slice(0, words.size-1).map(EPSUtils.strToInstance(_))
      val output: Any = EPSUtils.strToInstance(words.last)
      new Test(input, output)
    }
    (tests, varDefs.slice(0, varDefs.size-1), varDefs.slice(varDefs.size-1, varDefs.size))
  }
}