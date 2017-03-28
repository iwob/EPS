

import org.junit.Test
import org.junit.Assert._
import eps.smt.SmtlibUtils
import swim.tree.Op
import swim.Grammar
import eps.core.VarDef

final class TestSmtlibUtils {
  val g = Grammar('S,
      'S -> List('x, 'y,
        '+ -> ('S, 'S),
        '- -> ('S, 'S),
        '* -> ('S, 'S),
        'div -> ('S, 'S),
        'ite -> ('SB, 'S, 'S)
      ),
      'SB -> List('a, 'b,
        '< -> ('S, 'S),
        '<= -> ('S, 'S),
        '> -> ('S, 'S),
        '>= -> ('S, 'S),
        '= -> ('S, 'S),
        '= -> ('SB, 'SB)))
  
  @Test def test_smtlibToOp_pureTerminals() {
    assertEquals(Op(true), SmtlibUtils.smtlibToOp("true"))
    assertEquals(Op('SB, true), SmtlibUtils.smtlibToOp("true", Some(g)))
    assertEquals(Op(5), SmtlibUtils.smtlibToOp("5"))
    assertEquals(Op('S, 5), SmtlibUtils.smtlibToOp("5", Some(g)))
    assertEquals(Op('x), SmtlibUtils.smtlibToOp("x"))
    assertEquals(Op('S, 'x), SmtlibUtils.smtlibToOp("x", Some(g)))
  }
  
  @Test def test_smtlibToOp_specialCases() {
    assertEquals(Op('asd), SmtlibUtils.smtlibToOp("asd"))
    assertEquals(Op('default, 'asd), SmtlibUtils.smtlibToOp("asd", Some(g))) // asd is not in the grammar.
    val varDefs = List(VarDef('asd, "Int"))
    assertEquals(Op('S, 'asd), SmtlibUtils.smtlibToOp("asd", Some(g), Some(varDefs))) // asd is not in the grammar, but we provide varDefs
  }
  
  @Test def test_smtlibToOp_nonterminals() {
    assertEquals(Op('+, Op('x), Op('y)), SmtlibUtils.smtlibToOp("(+ x y)"))
    assertEquals(Op('S, '+, Op('S, 'x), Op('S, 'y)), SmtlibUtils.smtlibToOp("(+ x y)", Some(g)))
    assertEquals(Op('+, Op('x), Op('*, Op(5), Op(6))), SmtlibUtils.smtlibToOp("(+ x (* 5 6))"))
    assertEquals(Op('S, '+, Op('S, 'x), Op('S, '*, Op('S, 5), Op('S, 6))), SmtlibUtils.smtlibToOp("(+ x (* 5 6))", Some(g)))
    assertEquals(Op('+, Op('div, Op('a), Op('b)), Op('*, Op(5), Op(6))), SmtlibUtils.smtlibToOp("(+ (div a b) (* 5 6))"))
    assertEquals(Op('S, '+, Op('S, 'div, Op('SB, 'a), Op('SB, 'b)), Op('S, '*, Op('S, 5), Op('S, 6))), SmtlibUtils.smtlibToOp("(+ (div a b) (* 5 6))", Some(g)))
    assertEquals(Op('+, Op('x), Op('*, Op(5), Op('div, Op(6), Op('y)))), SmtlibUtils.smtlibToOp("(+ x (* 5 (div 6 y)))"))
    assertEquals(Op('S, '+, Op('S, 'x), Op('S, '*, Op('S, 5), Op('S, 'div, Op('S, 6), Op('S, 'y)))), SmtlibUtils.smtlibToOp("(+ x (* 5 (div 6 y)))", Some(g)))
  }
  
  @Test def test_getNtOfGrammarSymbol() {
    assertEquals(None, SmtlibUtils.getNtOfGrammarSymbol(g, 'S))
    assertEquals(None, SmtlibUtils.getNtOfGrammarSymbol(g, 'SB))
    assertEquals(None, SmtlibUtils.getNtOfGrammarSymbol(g, 'add))
    assertEquals(Some('S), SmtlibUtils.getNtOfGrammarSymbol(g, '+))
    assertEquals(Some('S), SmtlibUtils.getNtOfGrammarSymbol(g, 'div))
    assertEquals(Some('SB), SmtlibUtils.getNtOfGrammarSymbol(g, '<))
    assertEquals(Some('SB), SmtlibUtils.getNtOfGrammarSymbol(g, '=))
    assertEquals(Some('S), SmtlibUtils.getNtOfGrammarSymbol(g, 'x))
    assertEquals(Some('SB), SmtlibUtils.getNtOfGrammarSymbol(g, 'a))
  }
}