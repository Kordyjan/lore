package lore

import quoted.*

transparent inline def taskTransform[C, T](inline block: C ?=> T): Any = ${
  taskTransformImpl[C, T]('{ block })
}

private def taskTransformImpl[C: Type, T: Type](block: Expr[C ?=> T])(using
    Quotes
): Expr[Any] = internal[C, T](block).asExpr

private def internal[C: Type, T: Type](block: Expr[C ?=> T])(using Quotes) =
  import quotes.reflect.*

  class TypeCollector(productEvidence: Symbol)
      extends TreeAccumulator[List[TypeRepr]]:
    def foldTree(x: List[TypeRepr], tree: Tree)(owner: Symbol): List[TypeRepr] =
      tree match
        case Apply(fun, params) =>
          val MethodType(_, tps, _) = fun.tpe.widen: @unchecked
          (params zip tps).foldRight(foldTree(x, fun)(owner))(
            searchParam(owner)
          )
        case _ =>
          foldOverTree(x, tree)(owner)

    def searchParam(owner: Symbol)(
        param: (Term, TypeRepr),
        x: List[TypeRepr]
    ): List[TypeRepr] = param._1 match
      case r: Ref if r.symbol == productEvidence =>
        param._2 :: x // TODO: check for duplicates
      case t =>
        foldTree(x, t)(owner)
  end TypeCollector

  def transformBody(productEvidence: Symbol, owner: Symbol, body: Term)(
      resolver: TypeRepr => Term
  ): Term =
    object BodyTransformer extends TreeMap:
      var evidences = List.empty[(TypeRepr, Symbol)]

      override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
        case Apply(fun, params) =>
          val MethodType(_, tps, _) = fun.tpe.widen: @unchecked
          val newParams = (params zip tps).map(transformParam(owner))
          Apply(transformTerm(fun)(owner), newParams)
        case _ =>
          super.transformTerm(tree)(owner)

      private def transformParam(owner: Symbol)(
          value: Term,
          tpe: TypeRepr
      ): Term =
        value match
          case r: Ref if r.symbol == productEvidence =>
            resolver(tpe)
          case _ => transformTerm(value)(owner)
    end BodyTransformer

    BodyTransformer.transformTerm(body)(owner)
  end transformBody

  block.asTerm match
    case i @ Inlined(
          _,
          _,
          b @ Block(
            (d @ DefDef(
              _,
              TermParamClause(par :: Nil) :: Nil,
              tpt,
              Some(body)
            )) :: Nil,
            c @ Closure(_, _)
          )
        ) =>
      val types = TypeCollector(par.symbol)
        .foldTree(Nil, body)(Symbol.spliceOwner)
        .distinct
        .sortBy(nameType)

      def fun = contextFunction(d.name, types, tpt.tpe)(
        transformBody(par.symbol, d.symbol.owner, body)
      )
      val union = types.reduce(AndType(_, _)).simplified
      val classSymbol = Symbol.requiredClass("lore.Using")
      val instanceType =
        classSymbol.typeRef.appliedTo(tpt.tpe :: union :: Nil)
      New(Inferred(instanceType))
        .select(classSymbol.primaryConstructor)
        .appliedToTypes(tpt.tpe :: union :: Nil)
        .appliedTo(fun)
    case t => throw AssertionError("Unsupported tree:\n" + t.show)
end internal

private def contextFunction(using Quotes)(
    name: String,
    types: List[quotes.reflect.TypeRepr],
    result: quotes.reflect.TypeRepr
)(
    body: (
        quotes.reflect.TypeRepr => quotes.reflect.Term
    ) => quotes.reflect.Term
): quotes.reflect.Term =
  import quotes.reflect.*

  val tupleSymbol: Symbol = defn.TupleClass(types.length)
  val tupleType = tupleSymbol.typeRef.appliedTo(types)

  val methodSymbol =
    val tpe =
      MethodType("$contextTuple" :: Nil)(_ => tupleType :: Nil, _ => result)
    Symbol.newMethod(Symbol.spliceOwner, name, tpe)

  val defDef =
    def rhs(params: List[List[Tree]]): Option[Term] =
      def resolver(t: TypeRepr) =
        val param = params.head.collectFirst { case t: Term => t }.get
        val idx = types.indexWhere(_ =:= t.widen) + 1
        val accessor = tupleSymbol.methodMember(s"_$idx").head
        param.select(accessor)
      Some(body(resolver))
    DefDef(methodSymbol, rhs)

  Block(defDef :: Nil, Closure(Ref(methodSymbol), None))

inline def nameTypeM[T]: String = ${nameTypeMImpl[T]}

def nameTypeMImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect.*

  val res = linearize(TypeRepr.of[T]).map(_.show).mkString("(", ", ", ")")
  Literal(StringConstant(res)).asExprOf[String]

def linearize(using Quotes)(tpe: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
  import quotes.reflect.*
  def rec(tpe: TypeRepr) =
    tpe match
      case AndType(left, right) => linearize(left) ::: linearize(right)
      case t => t :: Nil
  rec(tpe.dealias.simplified).sortBy(nameType)

def nameType(using Quotes)(tpe: quotes.reflect.TypeRepr): String =
  import quotes.reflect.*
  "$" + tpe.dealias.simplified.show.replaceAll("\\.", "\\$")
