package test

import compiletime.testing.*

trait CompilationAssertions extends munit.Assertions:
  transparent inline def assertCompiles(inline code: String) =
    val errors = typeCheckErrors(code)
    assertEquals(clue(errors), Nil)

  transparent inline def assertError(inline code: String, inline prefix: String) =
    val errors = typeCheckErrors(code)
    assertEquals(clue(errors).length, 1)
    assert(errors.head.message.startsWith(prefix))