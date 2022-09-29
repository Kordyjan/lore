package lore

import quoted.*

class Using[R, -C](private val block: Any):
  transparent inline def run: Any = ${ runImpl[C, R]('{ block }) }

def runImpl[C: Type, R: Type](block: Expr[Any])(using Quotes) =
  import quotes.reflect.*

  val args = linearize(TypeRepr.of[C])
  val result = TypeRepr.of[R]

  val symbol =
    val names = args.map(nameType)
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
      val preservedType = defn.FunctionClass(1).typeRef.appliedTo(tupleClass.typeRef.appliedTo(args) :: TypeRepr.of[R] :: Nil)
      val cast = defn.AnyClass.methodMember("asInstanceOf").head
      val applyMethod = defn.FunctionClass(1).methodMember("apply").head
      Some(block.asTerm.select(cast).appliedToType(preservedType).select(applyMethod).appliedTo(tuple))
    DefDef(symbol, rhs)

  val finalTpe = defn
    .FunctionClass(args.length, true, false)
    .typeRef
    .appliedTo(args :+ result)
  Block(defDef :: Nil, Closure(Ref(symbol), Some(finalTpe))).asExpr
