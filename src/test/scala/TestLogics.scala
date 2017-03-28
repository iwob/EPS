

import org.junit.Test
import org.junit.Assert._
import eps.smt.LogicsCommon
import eps.smt.NIA
import eps.core.VarDef
import eps.utils.OptionsEPS
import fuel.util.Options
import fuel.util.Rng
import swim.tree.ConstantProvider
import eps.smt.HoleDef

final class TestLogics {
  @Test def test_getTerminalsLists() {
    val hVarInt = HoleDef('varInt, "Int", "((Start Int (x y (+ Start Start))))")
    val hConstInt = HoleDef('constInt, "Int")
    val holesDefs = List(hVarInt, hConstInt)
    val vars = List(VarDef('x, "Int"), VarDef('y, "Int"), VarDef('a, "Bool"))
    
    def test1() {
      val env = OptionsEPS(Options("--eps.useConstantProvider true " +
                                   "--eps.useInputVarsAsTerminals true"))
      val terminalsMap = LogicsCommon.getTerminalsLists(NIA.prodTypes, vars, holesDefs)(env, Rng(env.options))
      assertEquals(5, terminalsMap('S).size)
      assertEquals(1, terminalsMap('S).filter(_.isInstanceOf[ConstantProvider]).size) // 1 constant provider
      assertEquals(2, terminalsMap('S).filter{ x => x == 'varInt || x == 'constInt}.size) // 2 holes
      assertEquals(2, terminalsMap('S).filter{ x => x == 'x || x == 'y}.size) // 2 variables
      assertEquals(1, terminalsMap('SB).size)
      assertEquals(1, terminalsMap('SB).filter{ x => x == 'a}.size) // 1 variable
    }
    def test2() {
      val env = OptionsEPS(Options("--eps.useConstantProvider false " +
                                   "--eps.useInputVarsAsTerminals false"))
      val terminalsMap = LogicsCommon.getTerminalsLists(NIA.prodTypes, vars, holesDefs)(env, Rng(env.options))
      assertEquals(2, terminalsMap('S).size)
      assertEquals(0, terminalsMap('S).filter(_.isInstanceOf[ConstantProvider]).size)
      assertEquals(2, terminalsMap('S).filter{ x => x == 'varInt || x == 'constInt}.size)
      assertEquals(0, terminalsMap('S).filter{ x => x == 'x || x == 'y}.size)
      assertEquals(0, terminalsMap('SB).size)
      assertEquals(0, terminalsMap('SB).filter{ x => x == 'a}.size)
    }
    
    test1()
    test2()
  }
}