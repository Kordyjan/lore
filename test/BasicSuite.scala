package test

import lore.*
import utils.*
import compiletime.testing.typeCheckErrors

class BasicSuite extends munit.FunSuite:

  test("two context elements") {
    val work = task {
      obtainB + obtainA
    }

    val res1 =
      given A = A(5)
      given B = B("test")

      work.run
    assertEquals(res1, "test5")
  }

  test("two context elements, one missing, not compiling") {
    val work = task {
      obtainB + obtainA
    }

    given A = A(5)

    assert {
      val errors = clue(typeCheckErrors("work.run"))
      errors.size == 1 && errors.head.message.startsWith(
        "No given instance of type tests.utils.B was found"
      )
    }
  }
