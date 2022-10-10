package tests

import lore.*
import utils.*
import language.experimental.fewerBraces

class BasicSuite extends munit.FunSuite:
  test("two context elements"):
    val work = task:
      obtainB + obtainA

    val res1 =
      given A = A(5)
      given B = B("test")
      work.run
    assertEquals(res1, "test5")

  test("two context elements, one missing, not compiling"):
    val work = task:
      obtainB + obtainA

    given A = A(5)
    println(compiletime.testing.typeCheckErrors("work.run"))


