package interpreter

import org.junit.Test
import org.junit.Assert._

import interpreter.Interpreter._

class TestsPrograms {
  val memory: Memory = Map(
    Var("X") -> CstValue("xxx"),
    Var("Y") -> CstValue("yyy"),
    Var("Z") -> NlValue
  )
  @Test
  def Test_interpreterMemorySet_1(): Unit = {
    val expected = Map(Var("X") -> CstValue("xxx"))
    val result = interpreterMemorySet(List(Var("X")), List(CstValue("xxx")))
    assertEquals(result, expected)
  }

  @Test
  def Test_interpreterMemorySet_2(): Unit = {
    val expected = memory
    val result = interpreterMemorySet(
      List(Var("X"), Var("Y"), Var("Z")),
      List(CstValue("xxx"), CstValue("yyy"), NlValue)
    )
    assertEquals(result, expected)
  }

  @Test
  def Test_interpreterMemoryGet_1(): Unit = {
    assertEquals(
      List(CstValue("xxx")),
      interpreterMemoryGet(
        List(Var("X")),
        memory
      )
    )
  }

  @Test
  def Test_interpreterMemoryGet_2(): Unit = {
    assertEquals(
      List(CstValue("xxx"), NlValue),
      interpreterMemoryGet(
        List(Var("X"), Var("Z")),
        memory
      )
    )
  }

  @Test
  def Test_interpreter(): Unit = {
    val reverse: Program =
      Progr(
        List(Var("X")),
        List(
          Set(Var("Y"), Nl),
          While(
            VarExp("X"),
            List(
              Set(Var("Y"), Cons(Hd(VarExp("X")), VarExp("Y"))),
              Set(Var("X"), Tl(VarExp("X")))
            )
          )
        ),
        List(Var("Y"))
      )
    assertEquals(
      List(
        ConsValue(
          CstValue("ddd"),
          ConsValue(
            CstValue("ccc"),
            ConsValue(CstValue("bbb"), ConsValue(CstValue("aaa"), NlValue))
          )
        )
      ),
      interpreter(
        reverse,
        List(
          ConsValue(
            CstValue("aaa"),
            ConsValue(
              CstValue("bbb"),
              ConsValue(CstValue("ccc"), ConsValue(CstValue("ddd"), NlValue))
            )
          )
        )
      )
    )
  }
}
