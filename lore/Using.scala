package lore

import quoted.*
import scala.annotation.showAsInfix

transparent inline def task[C, T](inline block: C ?=> T): T Using C = ${
  taskTransformImpl[C, T]('{ block })
}

@showAsInfix class Using[R, -C](
    private val block: Any,
    private val signature: List[String]
):
  transparent inline def run: Any = ${ runImpl[C, R]('block, 'signature) }

  def flatMap[T, C2](f: R => (T Using C2)): T Using C & C2 = task { f(run).run }
