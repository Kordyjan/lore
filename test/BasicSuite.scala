package test

import lore.*
import utils.*
import compiletime.testing.typeCheckErrors
import language.experimental.fewerBraces

class BasicSuite extends munit.FunSuite with CompilationAssertions:

    test("two context elements"):
      val work = task:
        obtainB + obtainA
      assertCompiles("summon[work.type <:< Using[String, A & B]]")

      val res =
        given A = A(5)
        given B = B("test")
        work.run
      assertEquals(res, "test5")

    test("two context elements, one missing, not compiling"):
      val work = task:
        obtainB + obtainA

      given A = A(5)
      assertError("work.run", "No given instance of type test.utils.B")

    test("part of context can be locally eliminated"):
      val work = task:
        given A = A(7)
        obtainB + obtainA

      val res =
        given A = A(5)
        given B = B("test")
        work.run

      assertEquals(res, "test7")
