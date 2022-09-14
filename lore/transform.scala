package lore

import quoted.*

transparent inline def task[C, T](inline block: C ?=> T): Any = ${
  taskImpl[C, T]('{ block })
}

private def taskImpl[C: Type, T: Type](block: Expr[C ?=> T])(using
    Quotes
): Expr[Any] =
  val x = internal[C, T](block)
  x.asExpr

private def internal[C: Type, T: Type](block: Expr[C ?=> T])(using Quotes) =
  import quotes.reflect.*

  class TypeCollector(productEvidence: Symbol)
      extends TreeAccumulator[List[TypeRepr]]:
    def foldTree(x: List[TypeRepr], tree: Tree)(owner: Symbol): List[TypeRepr] =
      tree match
        case Apply(fun, params) =>
          val MethodType(_, tps, _) = fun.tpe.widen: @unchecked
          (params zip tps).foldRight(foldTree(x, fun)(owner))(searchParam(owner))
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

  def transformBody(productEvidence: Symbol, owner: Symbol, body: Term)
  (resolver: TypeRepr => Term)
  : Term =
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

  object Mapper extends TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term = tree match
      case b @ Block(
            (d @ DefDef(
              _,
              TermParamClause(par :: Nil) :: Nil,
              tpt,
              Some(body)
            )) :: Nil,
            c @ Closure(_, _)
          ) =>
        val types = TypeCollector(par.symbol).foldTree(Nil, body)(Symbol.spliceOwner).distinct

        def fun = contextFunction(d.name, types, tpt.tpe)(transformBody(par.symbol, d.symbol.owner, body))

        fun
      case _ =>
        super.transformTerm(tree)(owner)
  end Mapper

  Mapper.transformTree(block.asTerm)(Symbol.spliceOwner)

private def contextFunction(using Quotes)(
    name: String,
    types: List[quotes.reflect.TypeRepr],
    result: quotes.reflect.TypeRepr
  )(
    body: (quotes.reflect.TypeRepr => quotes.reflect.Term) => quotes.reflect.Term
  ): quotes.reflect.Term =
  import quotes.reflect.*
  val symbol =
    val names = types.zipWithIndex.map { (_, id) => "$" + id.toString() }
    val tpe = MethodType(names)(_ => types, _ => result)
    Symbol.newMethod(Symbol.spliceOwner, name, tpe)

  val defDef =
    def rhs(params: List[List[Tree]]): Option[Term] =
      def resolver(t: TypeRepr) = params.head.collect {
        case term: Term =>
          // println(term.tpe.widen)
          term
      }.collectFirst { case term: Term if term.tpe.widen =:= t => term }.get
      Some(body(resolver))
    DefDef(symbol, rhs)

  val finalTpe = defn.FunctionClass(types.length, true, false).typeRef.appliedTo(types :+ result)
  Block(defDef :: Nil, Closure(Ref(symbol), Some(finalTpe)))

extension (using q: Quotes)(term: q.reflect.Term)
  def uninline: q.reflect.Term = term match
    case q.reflect.Inlined(_, _, inner) => inner
    case t => t

  def unclosure: q.reflect.DefDef =
    val q.reflect.Block((d: q.reflect.DefDef) :: Nil, _) = term: @unchecked
    d