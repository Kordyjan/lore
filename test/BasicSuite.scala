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

    test("part of context locally eliminated"):
      val work = task:
        given A = A(7)
        obtainB + obtainA

      assertCompiles("summon[work.type <:< (String Using B)]")

      val res =
        given A = A(5)
        given B = B("test")
        work.run

      assertEquals(res, "test7")

    test("entire context locally eliminated"):
      val work = task:
        given A = A(7)
        given B = B("abc")
        obtainB + obtainA

      assertCompiles("summon[work.type <:< (String Using Any)]")

      val res =
        given A = A(5)
        given B = B("test")
        work.run

      assertEquals(res, "abc7")

    test("context can be partially eliminated"):
      val work = task:
        obtainB + obtainA

      val work2 = task:
        given A = A(5)
        work.run

      assertCompiles("summon[work2.type <:< (String Using B)]")

      given B = B("xyz")
      val res = work2.run
      assertEquals(res, "xyz5")

    test("flatMap compiles"):
      val work1 = task:
        obtainA + obtainB

      val work2 = work1.flatMap: _ =>
        task:
          obtainC

      assertCompiles("summon[work2.type <:< (Unit Using A & B & C)]")
