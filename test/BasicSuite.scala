package test

import lore.*
import utils.*
import compiletime.testing.typeCheckErrors
import language.experimental.fewerBraces

class BasicSuite extends munit.FunSuite:

  test("two context elements"):
    val work = task:
      obtainB + obtainA

    val res =
      given A = A(5)
      given B = B("test")

      work.run
    assertEquals(res, "test5")

  test("two context elements, one missing, not compiling"):
    val work = task:
      obtainB + obtainA

    given A = A(5)
    val errors = clue(typeCheckErrors("work.run"))

    assertEquals(errors.size, 1)
    assertEquals(errors.head.message, "No given instance of type test.utils.B was found for parameter of (test.utils.A, test.utils.B) ?=> String")

  test("part of context can be locally eliminated"):
    val work = task:
      given A = A(7)
      obtainB + obtainA

    val res =
      given A = A(5)
      given B = B("test")
      work.run

    assertEquals(res, "test7")
