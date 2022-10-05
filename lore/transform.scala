package lore

import quoted.*

def taskTransformImpl[C: Type, T: Type](block: Expr[C ?=> T])(using
    Quotes
): Expr[T Using C] = internal[C, T](block).asExprOf[T Using C]

private def internal[C: Type, T: Type](block: Expr[C ?=> T])(using Quotes): quotes.reflect.Term =
  import quotes.reflect.*
  val ext = extend
  import ext.*

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
      val usingClass = EClass.of("lore.Using")
      usingClass.createInstance(fun :: Nil)
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

def runImpl[C: Type, R: Type](block: Expr[Any])(using Quotes) =
  import quotes.reflect.*
  val ext = extend
  import ext.*

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
      val applyMethod = defn.FunctionClass(1).methodMember("apply").head
      Some(block.asTerm.cast(preservedType).select(applyMethod).appliedTo(tuple))
    DefDef(symbol, rhs)

  val finalTpe = defn
    .FunctionClass(args.length, true, false)
    .typeRef
    .appliedTo(args :+ result)
  Block(defDef :: Nil, Closure(Ref(symbol), Some(finalTpe))).asExpr

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
