

import org.junit.Assert._
import org.junit.Test
import fuel.util.Options
import eps.core.Evaluation
import eps.core.ProblemDefinition
import eps.smt.Holes
import eps.smt.SmtlibUtils
import eps.smt.SolverResult
import eps.utils.OptionsEPS
import swim.tree.Op
import eps.smt.RunnerPySV
import eps.smt.NIA
import eps.core.VarDef

final class TestEvaluation {
  @Test def test_getDivSafetyConditions() {
    val list0 = SmtlibUtils.getDivSafetyConditions(Op.fromStr("y"))
    assertEquals(Set(), list0.toSet)
    val list1 = SmtlibUtils.getDivSafetyConditions(Op.fromStr("/(x y)"))
    assertEquals(Set("(assert (not (= 0 y)))"), list1.toSet)
    val list2 = SmtlibUtils.getDivSafetyConditions(Op.fromStr("/(x /(y x))"))
    assertEquals(Set("(assert (not (= 0 (/ y x))))", "(assert (not (= 0 x)))"), list2.toSet)
    val list3 = SmtlibUtils.getDivSafetyConditions(Op.fromStr("/(/(x y) /(y x))"))
    assertEquals(Set("(assert (not (= 0 (/ y x))))", "(assert (not (= 0 y)))", "(assert (not (= 0 x)))"), list3.toSet)
    val list4 = SmtlibUtils.getDivSafetyConditions(Op.fromStr("+(/(x 0) /(x 7))"))
    assertEquals(Set(), list4.toSet)
  }
  
  @Test def test_isOpValid() {
    assertEquals(false, Evaluation.isOpValid(Op.fromStr("+(5 div(x 0))")))
    assertEquals(false, Evaluation.isOpValid(Op.fromStr("div(x 0)")))
    assertEquals(true, Evaluation.isOpValid(Op.fromStr("div(0 x)")))
  }
  
  @Test def test_evalNormally_error() {
    val op = Op.fromStr("div(x 0)")
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/int1.csv --eps.holesConsts constInt:Int  --eps.optimizeNumPassedTestCases false"))
    val problem = ProblemDefinition(env)
    val fit = Evaluation.evalNormally(op, problem)
    assertEquals(0, fit.value)
    assertEquals(None, fit.solverRes)
  }
  
  @Test def test_evalSymbConsts_sat() {
    val op = Op.fromStr("*(constInt +(x y))")
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/int1.csv --eps.holesConsts constInt:Int  --eps.optimizeNumPassedTestCases false"))
    val problem = ProblemDefinition(env)
    val fit = Evaluation.evalHoles(op, problem, RunnerPySV(problem))
    assertEquals(7, fit.value)
    assertEquals(SolverResult.SAT, fit.solverRes.get.decision)
    assertEquals(true, fit.solverRes.isDefined)
    assertEquals("2", fit.solverRes.get.model.get.getOrElse("constInt0", ""))
  }
  
  @Test def test_evalSymbConsts_unsat() {
    val op = Op.fromStr("+(constInt +(x y))")
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/int1.csv --eps.holesConsts constInt:Int --eps.optimizeNumPassedTestCases false"))
    val problem = ProblemDefinition(env)
    val fit = Evaluation.evalHoles(op, problem, RunnerPySV(problem))
    assertEquals(0, fit.value)
    assertEquals(SolverResult.UNSAT, fit.solverRes.get.decision)
    assertEquals(true, fit.solverRes.isDefined)
    assertEquals(false, fit.solverRes.get.model.isDefined)
    assertEquals(false, fit.solverRes.get.assignments.isDefined)
  }
  
  @Test def test_evalSymbConsts_checkingSubstitution() {
    val op = Op.fromStr("*(ite(>=(-(x y) +(x constInt)) constInt y) -(-(constInt ite(>(y constInt) x constInt)) +(-(y constInt) *(y constInt))))")
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/int1.csv --eps.holesConsts constInt:Int  --eps.optimizeNumPassedTestCases false"))
    val problem = ProblemDefinition(env)
    val fit = Evaluation.evalHoles(op, problem, RunnerPySV(problem))
    assertEquals(7, fit.value)
    assertEquals(SolverResult.SAT, fit.solverRes.get.decision)
    assertEquals(true, fit.solverRes.isDefined)
    assertEquals(true, fit.solverRes.get.model.isDefined)
    
    val (renamedOp, holeNamesTypes) = Holes.renameHoles(op, problem.holesDefs)
    val substOp = Holes.fillHoles(renamedOp, fit.solverRes.get, holeNamesTypes)
    println("AFTER SUBSTITUTION: " + substOp)
    val fit2 = Evaluation.evalNormally(substOp, problem)
    assertEquals(7, fit2.value)
    assertEquals(false, fit2.solverRes.isDefined)
  }
  
  @Test def test_evalHoles_bisection_error() {
    val op = Op.fromStr("(+ div(2 0) *(constInt +(x y)))")
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/int1.csv --eps.holesConsts constInt:Int --eps.optimizeNumPassedTestCases true --eps.optimizationMode bisecting"))
    val problem = ProblemDefinition(env)
    val fit = Evaluation.evalHoles(op, problem, RunnerPySV(problem))
    assertEquals(0, fit.value)
    assertEquals(true, fit.solverRes.isDefined)
    assertEquals(SolverResult.INVALID, fit.solverRes.get.decision)
  }
  
  @Test def test_evalHoles_bisection0() {
    // Test for bisection when optimal program may be found.
    val op = Op.fromStr("*(constInt +(x y))")
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/int1.csv --eps.holesConsts constInt:Int --eps.optimizeNumPassedTestCases true --eps.optimizationMode bisecting"))
    val problem = ProblemDefinition(env)
    val fit = Evaluation.evalHoles(op, problem, RunnerPySV(problem))
    assertEquals(7, fit.value)
    assertEquals(SolverResult.SAT, fit.solverRes.get.decision)
    assertEquals(true, fit.solverRes.get.model.isDefined)
    assertEquals("2", fit.solverRes.get.model.get("constInt0"))
  }
  
  @Test def test_evalHoles_bisection1() {
    // Test for bisection when only intermediate program may be found.
    // Optimal value for constInt is 4 and leads to 3 passed test cases.
    val op = Op.fromStr("*(constInt x)")
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/int1_simple.csv --eps.holesConsts constInt:Int --eps.optimizeNumPassedTestCases true --eps.optimizationMode bisecting"))
    val problem = ProblemDefinition(env)
    val fit = Evaluation.evalHoles(op, problem, RunnerPySV(problem))
    assertEquals(3, fit.value)
    assertEquals(SolverResult.SAT, fit.solverRes.get.decision)
    assertEquals(true, fit.solverRes.get.model.isDefined)
    assertEquals("4", fit.solverRes.get.model.get("constInt0"))
  }
  
  @Test def test_evalHoles_bisection2() {
    // Test for bisection when fitness > 0 cannot be achieved.
    val op = Op.fromStr("+(*(+(x 100) constInt) 1)")
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/x2-1.csv --eps.holesConsts constInt:Int --eps.optimizeNumPassedTestCases true --eps.optimizationMode bisecting"))
    val problem = ProblemDefinition(env)
    val fit = Evaluation.evalHoles(op, problem, RunnerPySV(problem))
    assertEquals(0, fit.value)
    assertEquals(true, fit.solverRes.isDefined)
    assertEquals(SolverResult.UNKNOWN, fit.solverRes.get.decision)
  }
  
  @Test def test_evalHoles_varHoles() {
    val op = Op.fromStr("*(2 +(varInt y))")
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/int1.csv --eps.holesVars varInt:Int:x,y --eps.optimizeNumPassedTestCases false"))
    val problem = ProblemDefinition(env)
    val fit = Evaluation.evalHoles(op, problem, RunnerPySV(problem))
    println("[test_evalHoles_varHoles] model: " + fit.solverRes.get.model.get)
    assertEquals(7, fit.value)
    assertEquals(SolverResult.SAT, fit.solverRes.get.decision)
    assertEquals(true, fit.solverRes.get.model.isDefined)
    assertEquals("0", fit.solverRes.get.model.get("varInt0Start0_r0"))
    assertEquals(true, fit.solverRes.get.holesContent.isDefined)
    assertEquals("x", fit.solverRes.get.holesContent.get("varInt0"))
  }
  
  @Test def test_eval_issue1() {
    // Test for issue with differing normal and computed by solver fitness.
    val op = Op.fromStr("*(varInt +(+(constInt ite(>(div(-(constInt constInt) +(constInt varInt)) -(-(constInt varInt) constInt)) constInt varInt)) ite(<(5 varInt) constInt varInt)))")
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/int1_simple.csv --eps.holesConsts constInt:Int " +
        "--eps.optimizeNumPassedTestCases true --eps.optimizationMode bisecting --eps.holesVars varInt:Int:x,y " +
        "--eps.safetyConditions true --eps.useInputVarsAsTerminals false"))
    val problem = ProblemDefinition(env)
    val pysv = RunnerPySV(problem)
    val fitSolver = Evaluation.evalHoles(op, problem, pysv)
    assertEquals(5, fitSolver.value)
    
    val opFilled = Holes.fillHolesInOp(op, fitSolver, problem)
    val fitNormal = Evaluation.evalNormally(opFilled, problem, doPrint=false)
    // If obtained fitness is here different than 7, then most probably definition of division differs between GP and SMT solver.
    // Definition of div in SMT-LIB: http://smtlib.cs.uiowa.edu/theories-Ints.shtml
    assertEquals(5, fitNormal.value)
  }
  
  @Test def test_eval_issue2() {
    // Test for issue with differing normal and computed by solver fitness.
    val op = Op.fromStr("*(varInt +(+(constInt ite(=(4 varInt) constInt varInt)) ite(<(5 varInt) constInt varInt)))")
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/int1.csv --eps.holesConsts constInt:Int " +
        "--eps.optimizeNumPassedTestCases true --eps.optimizationMode solver --eps.holesVars varInt:Int:x,y " +
        "--eps.safetyConditions true --eps.useInputVarsAsTerminals false"))
    val problem = ProblemDefinition(env)
    val pysv = RunnerPySV(problem)
    val fitSolver = Evaluation.evalHoles(op, problem, pysv)
    assertEquals("varInt1", fitSolver.solverRes.get.holesContent.get("varInt1")) // Hole remains in the solution.
    val opFilled = Holes.fillHolesInOp(op, fitSolver, problem)
    assertEquals(false, opFilled.toString().contains("varInt"))
  }
  
  @Test def test_eval_issue3() {
    val op = Op.fromStr("*(ite(=(<=(*(x x) x) >(1 ite(=(<(y varInt) <=(*(x x) x)) y y))) ite(<=(*(-(x 4) x) x) y 1) *(div(-(y ite(<(1 x) ite(<(+(x x) +(x x)) -(5 ite(>=(x x) x y)) *(+(4 y) 2)) 0)) 3) 3)) y)")
    // eval: 17, evalNormally: 0
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/keijzer12.csv --eps.holesConsts constInt:Int " +
        "--eps.optimizeNumPassedTestCases true --eps.optimizationMode solver --eps.holesVars varInt:Int:x,y " +
        "--eps.safetyConditions true"))
    val problem = ProblemDefinition(env)
    val pysv = RunnerPySV(problem)
    val fitSolver = Evaluation.evalHoles(op, problem, pysv)
    assertEquals(true, fitSolver.wasSolverUsed)
    assertEquals(SolverResult.SAT, fitSolver.solverRes.get.decision)
    assertEquals(17, fitSolver.value)
    val opFilled = Holes.fillHolesInOp(op, fitSolver, problem)
    val fitNormally = Evaluation.evalNormally(opFilled, problem, doPrint=true)
    assertEquals(false, opFilled.toString().contains("varInt"))
    assertEquals(17, fitNormally.value)
  }
  
  @Test def test_eval_issue4() {
    val env = OptionsEPS(Options("--eps.logic NIA --eps.pathTests data/int/keijzer12.csv --eps.useConstantProvider true " +
        "--eps.optimizeNumPassedTestCases true --eps.optimizationMode solver --eps.holesVars varInt:Int:x,y " +
        "--eps.safetyConditions true"))
    val problem = ProblemDefinition(env)
    // Fitness of the program with holes.
    val opOrig = Op.fromStr("-(*(ite(>(ite(>(varInt varInt) 0 varInt) varInt) varInt varInt) ite(>=(ite(<=(1 varInt) *(varInt 3) *(3 3)) +(varInt *(0 ite(>(1 varInt) 0 0)))) varInt 3)) ite(>(varInt -(ite(>(varInt varInt) 0 varInt) ite(>(varInt 1) -(2 ite(>(varInt varInt) *(varInt varInt) varInt)) *(varInt varInt)))) +(varInt varInt) 0))")
    val pysv = RunnerPySV(problem)
    val fitOrig = Evaluation.evalHoles(opOrig, problem, pysv)
    assertEquals(SolverResult.SAT, fitOrig.solverRes.get.decision)
    assertEquals(18, fitOrig.value) // 18 was reported in the logs.
    val opFilled = Holes.fillHolesInOp(opOrig, fitOrig, problem)
    // Historically after filling it was like this:
    // -(*(ite(>(ite(>(x y) 0 x) x) x y) ite(>=(ite(<=(1 y) *(x 3) *(3 3)) +(x *(0 ite(>(1 varInt) 0 0)))) y 3)) ite(>(y -(ite(>(x x) 0 x) ite(>(x 1) -(2 ite(>(x y) *(y x) y)) *(y x)))) +(y y) 0))
    assertEquals(false, opFilled.toString().contains("varInt")) // Now should not contain hole names, because default value is assigned.
    val fitNormally = Evaluation.evalNormally(opFilled, problem, doPrint=true)
    assertEquals(18, fitNormally.value)
  }
  
  @Test def test_nia_division() {
    val domain1 = NIA.getDomain(List(VarDef('x, "Int")), smtlibDiv=true)
    val semantics1 = domain1.semantics(List(List(1)))
    assertEquals(-1, semantics1( Op.fromStr("div(-1 2)")))
    assertEquals(0, semantics1( Op.fromStr("div(1 2)")))
    assertEquals(1, semantics1( Op.fromStr("div(-1 -2)")))
    assertEquals(0, semantics1( Op.fromStr("div(1 -2)")))
    
    val domain2 = NIA.getDomain(List(VarDef('x, "Int")), smtlibDiv=false)
    val semantics2 = domain2.semantics(List(List(1)))
    assertEquals(0, semantics2( Op.fromStr("div(-1 2)")))
    assertEquals(0, semantics2( Op.fromStr("div(1 2)")))
    assertEquals(0, semantics2( Op.fromStr("div(-1 -2)")))
    assertEquals(0, semantics2( Op.fromStr("div(1 -2)")))
  }
}