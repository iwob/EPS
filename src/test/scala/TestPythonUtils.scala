

import org.junit.Test
import org.junit.Assert._
import eps.smt.PythonUtils
import eps.smt.PySV


final class TestPythonUtils {
  val tests = List(swim.Test(List(1, 1), 4),
                   swim.Test(List(1, 2), 6),
                   swim.Test(List(1, 0), 2),
                   swim.Test(List(2, 2), 8))

  @Test def test_pyDictToScalaMap() {
    assertEquals(Map(), PythonUtils.pyDictToScalaMap("{}"))
    assertEquals(Map("a"->"1"), PythonUtils.pyDictToScalaMap("{'a':'1'}"))
    assertEquals(Map("a"->"1", "b"->"2"), PythonUtils.pyDictToScalaMap("{'a':'1', 'b':'2'}"))
    assertEquals(Map("a"->"1", "b"->"2"), PythonUtils.pyDictToScalaMap("{'a':'1','b':'2'}"))
  }
  
  @Test def test_tests2py_1() {
    val text = PySV.tests2py(tests, Seq("X0", "X1"), Seq("res"))
    assertEquals("([([1,1], [4]),  ([1,2], [6]),  ([1,0], [2]),  ([2,2], [8])],    ['X0','X1'], ['res'])", text)
  }
  
  
  @Test def test_tests2py_2() {
    val text = PySV.tests2py(tests, Seq("X0", "X1"), Seq("res1", "res2"))
    assertEquals("([([1,1], [4]),  ([1,2], [6]),  ([1,0], [2]),  ([2,2], [8])],    ['X0','X1'], ['res1','res2'])", text)
  }
}