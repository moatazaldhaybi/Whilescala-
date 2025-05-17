package interpreter

import org.junit.Test
import org.junit.Assert._

import interpreter.Interpreter._

class TestsMemoire {
  val memory: Memory = Map(
    Var("X") -> CstValue("xxx"),
    Var("Y") -> CstValue("yyy"),
    Var("Z") -> NlValue
  )
  val empty = Map.empty[Variable, Value]

  @Test
  def Test_lookUp_positive1(): Unit = {
    assertEquals(CstValue("xxx"), lookUp(Var("X"), memory))
  }

  @Test
  def Test_lookUp_positive2(): Unit = {
    assertEquals(CstValue("yyy"), lookUp(Var("Y"), memory))
  }

  @Test
  def Test_lookUp_default1(): Unit = {
    assertEquals(NlValue, lookUp(Var("X"), empty))
  }

  @Test
  def Test_lookUp_default2(): Unit = {
    assertEquals(NlValue, lookUp(Var("W"), memory))
  }

  @Test
  def Test_assign_positive1(): Unit = {
    val expected = Map(
      Var("X") -> CstValue("www"),
      Var("Y") -> CstValue("yyy"),
      Var("Z") -> NlValue
    )
    val result = assign(Var("X"), CstValue("www"), memory)
    assertEquals(result, expected)
  }

  @Test
  def Test_assign_positive2(): Unit = {
    val expected = Map(
      Var("X") -> CstValue("xxx"),
      Var("Y") -> CstValue("www"),
      Var("Z") -> NlValue
    )
    val result = assign(Var("Y"), CstValue("www"), memory)
    assertEquals(result, expected)
  }

  @Test
  def Test_assign_default1(): Unit = {
    val expected = memory + (Var("W") -> CstValue("www"))
    val result = assign(Var("W"), CstValue("www"), memory)
    assertEquals(result, expected)
  }

  @Test
  def Test_assign_default2(): Unit = {
    val expected = empty + (Var("W") -> CstValue("www"))
    val result = assign(Var("W"), CstValue("www"), empty)
    assertEquals(result, expected)
  }

}
