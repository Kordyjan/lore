package lore

import quoted.*

class Using[R, C](private val block: C => R):
  transparent inline def run: Any = ${ runImpl[C, R]('{ block }) }

def runImpl[C: Type, R: Type](block: Expr[C => R])(using Quotes) =
  import quotes.reflect.*

  val args = block.asTerm.tpe.widen.typeArgs.head.typeArgs
  val result = TypeRepr.of[R]

  val symbol =
    val names = args.zipWithIndex.map { (_, id) => "$" + id.toString }
    val tpe = MethodType(names)(_ => args, _ => result)
    Symbol.newMethod(Symbol.spliceOwner, "runImpl", tpe)

  val defDef =
    def rhs(params: List[List[Tree]]): Option[Term] =
      val values = params.head.collect { case t: Term => t }
      val tupleClass = defn.TupleClass(args.length)
      val tuple = New(Inferred(tupleClass.typeRef))
        .select(tupleClass.primaryConstructor)
        .appliedToTypes(args)
        .appliedToArgs(values)
      val applyMethod = defn.FunctionClass(1).methodMember("apply").head
      Some(block.asTerm.select(applyMethod).appliedTo(tuple))
    DefDef(symbol, rhs)

  val finalTpe = defn
    .FunctionClass(args.length, true, false)
    .typeRef
    .appliedTo(args :+ result)
  Block(defDef :: Nil, Closure(Ref(symbol), Some(finalTpe))).asExpr
