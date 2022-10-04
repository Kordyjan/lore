package lore

import quoted.*
import scala.annotation.showAsInfix

transparent inline def task[C, T](inline block: C ?=> T): T Using C = ${
  taskTransformImpl[C, T]('{ block })
}

@showAsInfix class Using[R, -C](private val block: Any):
  transparent inline def run: Any = ${ runImpl[C, R]('{ block }) }

