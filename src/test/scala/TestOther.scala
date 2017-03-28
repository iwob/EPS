

import org.junit.Assert._
import org.junit.Test
import eps.core.IterationData
import swim.tree.Op
import fuel.core.StatePop
import eps.core.FitnessEPS
import eps.smt.SolverResult


final class TestOther {
  @Test def test_IterationData_fitnessStats() {
    val pop = StatePop(List((Op('x), FitnessEPS(1, None, None)), (Op('x), FitnessEPS(2, None, None)), (Op('x), FitnessEPS(3, None, None))))
    val id = new IterationData(pop)
    val fitStats = id.seqStats(IterationData.FITNESS)
    assertEquals(true, Math.abs(fitStats._1 - 2) < 0.0001)
    assertEquals(true, Math.abs(fitStats._2 - 1) < 0.0001)
    assertEquals(true, Math.abs(fitStats._3 - 3) < 0.0001)
    assertEquals(true, Math.abs(fitStats._4 - 0.816496580928) < 0.0001)
  }
  
  @Test def test_IterationData_solverErrors() {
    val pop = StatePop(List(
        (Op('x), FitnessEPS(1, Some(SolverResult(SolverResult.SAT)), None)),
        (Op('x), FitnessEPS(2, Some(SolverResult(SolverResult.SAT)), None)),
        (Op('x), FitnessEPS(3, Some(SolverResult(SolverResult.UNSAT)), None))))
    val id = new IterationData(pop)
    assertEquals(2, id.numSat)
    assertEquals(1, id.numUnsat)
    assertEquals(0, id.numUnknown)
    assertEquals(0, id.numError)
    assertEquals(0, id.numInvalid)
    assertEquals(0, id.numTimeout)
  }
}