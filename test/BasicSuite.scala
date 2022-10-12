package test

import lore.*
import utils.*
import compiletime.testing.typeCheckErrors
import language.experimental.fewerBraces

import utest.*

object BasicSuite extends TestSuite:
  val tests = Tests:
    test("two context elements"):
      val work = task:
        obtainB + obtainA

      val res =
        given A = A(5)
        given B = B("test")

        work.run
      assert(res == "test5")

    test("two context elements, one missing, not compiling"):
      val work = task:
        obtainB + obtainA

      given A = A(5)
      val errors = typeCheckErrors("work.run")

      assert(errors.size == 1)
      assert(errors.head.message == "No given instance of type test.utils.B was found for parameter of (test.utils.A, test.utils.B) ?=> String")

    test("part of context can be locally eliminated"):
      val work = task:
        given A = A(7)
        obtainB + obtainA

      val res =
        given A = A(5)
        given B = B("test")
        work.run

      assert(res == "test7")
