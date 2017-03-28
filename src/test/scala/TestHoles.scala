

import org.junit.Test
import org.junit.Assert._
import swim.tree.Op
import eps.smt.Holes
import eps.smt.SolverResult
import eps.smt.HoleConstDef
import eps.smt.HoleDef
import eps.smt.HoleVarDef

final class TestHoles {
  val holes = List(('A,"Int"), ('B,"Bool")).map{ x => HoleConstDef(x._1, x._2)}
  @Test def test_renameHoles0() {
    val (op, list) = Holes.renameHoles(Op.fromStr("A"), holes)
    assertEquals(Op.fromStr("A0"), op)
    assertEquals("A0", list(0)._1)
    assertEquals('A, list(0)._2.name)
    assertEquals("Int", list(0)._2.tpe)
  }
  
  @Test def test_renameHoles1() {
    val (op, list) = Holes.renameHoles(Op.fromStr("+(A A)"), holes)
    assertEquals(Op.fromStr("+(A0 A1)"), op)
    assertEquals("A0", list(0)._1)
    assertEquals('A, list(0)._2.name)
    assertEquals("Int", list(0)._2.tpe)
    assertEquals("A1", list(1)._1)
    assertEquals('A, list(1)._2.name)
    assertEquals("Int", list(1)._2.tpe)
  }
    
  @Test def test_renameHoles2() {
    val (op, list) = Holes.renameHoles(Op.fromStr("*(+(A A) B B +(A B))"), holes)
    assertEquals(Op.fromStr("*(+(A0 A1) B0 B1 +(A2 B2))"), op)
    //assertEquals(List(("A0","Int"), ("A1","Int"), ("B0","Bool"),
    //                  ("B1","Bool"), ("A2","Int"), ("B2","Bool")), list)
    assertEquals(6, list.size)
    assertEquals("A0", list(0)._1)
    assertEquals('A, list(0)._2.name)
    assertEquals("A1", list(1)._1)
    assertEquals('A, list(1)._2.name)
    assertEquals("B0", list(2)._1)
    assertEquals('B, list(2)._2.name)
    assertEquals("B1", list(3)._1)
    assertEquals('B, list(3)._2.name)
    assertEquals("A2", list(4)._1)
    assertEquals('A, list(4)._2.name)
    assertEquals("B2", list(5)._1)
    assertEquals('B, list(5)._2.name)
  }
  
  @Test def test_fillHoles0() {
    val op = Op.fromStr("A0")
    val model = Map("A0"->"0")
    val solverRes = new SolverResult("sat", Some(model))
    val usedHoles = List(("A0", HoleDef('A, "Int")))
    val newOp = Holes.fillHoles(op, solverRes, usedHoles)
    assertEquals(Op.fromStr("0"), newOp)
  }
  
  @Test def test_fillHoles1() {
    val op = Op.fromStr("+(A0 A1)")
    val model = Map("A0"->"0", "A1"->"9")
    val solverRes = new SolverResult("sat", Some(model))
    val usedHoles = List(("A0",HoleDef('A, "Int")), ("A1",HoleDef('A, "Int")))
    val newOp = Holes.fillHoles(op, solverRes, usedHoles)
    assertEquals(Op.fromStr("+(0 9)"), newOp)
  }
  
  @Test def test_fillHoles2() {
    val hBool = HoleDef('B, "Bool")
    val hInt = HoleDef('A, "Int")
    val op = Op.fromStr("*(+(A0 A1) B0 B1 +(A2 B2))")
    val model = Map("A0"->"0", "A1"->"1", "A2"->"2",
                    "B0"->"false", "B1"->"true", "B2"->"true")
    val solverRes = new SolverResult("sat", Some(model))
    val usedHoles = List(("A0",hInt), ("A1",hInt), ("B0",hBool),
                         ("B1",hBool), ("A2",hInt), ("B2",hBool))
    val newOp = Holes.fillHoles(op, solverRes, usedHoles)
    assertEquals(Op.fromStr("*(+(0 1) false true +(2 true))"), newOp)
  }
  
  @Test def test_fillHoles3() {
    val hVarInt = HoleDef('varInt, "Int", "((Start Int (x y (+ Start Start))))")
    val hInt = HoleDef('A, "Int")
    val op = Op.fromStr("+(A0 varInt0)")
    val solverRes = new SolverResult("sat", Some(Map("A0"->"5")), holesContent=Some(Map("varInt0"->"(+ x y)")))
    val usedHoles = List(("A0",hInt), ("varInt0",hVarInt))
    val newOp = Holes.fillHoles(op, solverRes, usedHoles)
    assertEquals(Op.fromStr("+(5 +(x y))"), newOp)
  }
  
  @Test def test_fillHoles_defaultValues() {
    val hBool = HoleDef('bool, "Bool")
    val hInt = HoleDef('int, "Int")
    val hDouble = HoleDef('double, "Double")
    val op = Op.fromStr("ite(bool0 int0 double0)")
    val model = Map[String,String]()
    val solverRes = new SolverResult("sat", Some(model))
    val usedHoles = List(("bool0",hBool), ("int0",hInt), ("double0",hDouble))
    val newOp = Holes.fillHoles(op, solverRes, usedHoles)
    assertEquals(Op.fromStr("ite(false 0 0.0)"), newOp)
  }
  
  @Test def test_fillHoles_tooBigNumber() {
    val hInt = HoleDef('A, "Int")
    val op = Op.fromStr("+(A0 x)")
    val usedHoles = List(("A0",hInt))
    val solverRes = new SolverResult("sat", model=Some(Map("A0"->"47415637394397")))
    val newOp = Holes.fillHoles(op, solverRes, usedHoles)
    assertNotEquals(Op.fromStr("+(A x)"), newOp) // Not equal because of 'default in Op.fromStr
    assertEquals(Op('default, '+, Op('S, 'A), Op('default, 'x)), newOp)
  }
  
  @Test def test_HoleVarDef_createGrammar() {
    assertEquals("((Start Int (a b)))", HoleVarDef.createGrammar("Int", List("a", "b")))
  }
}