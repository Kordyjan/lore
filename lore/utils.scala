package lore

import quoted.*

def extend(using q: Quotes) = QuotesUtils[q.type]

class QuotesUtils[Q <: Quotes](using val q: Q):
  import q.reflect.*

  extension (term: Term) def cast(tpe: TypeRepr): Term =
    val method = defn.AnyClass.methodMember("asInstanceOf").head
    term.select(method).appliedToType(tpe)

  object ETuple:
    def fill(size: Int)(tpe: => TypeRepr) = of(List.fill(size)(tpe))
    def ofAny(size: Int) = fill(size)(defn.AnyClass.typeRef)
    def of(tpes: List[TypeRepr]) = EClass(defn.TupleClass(tpes.length), tpes)

  object EFunction:
    def of(tpes: List[TypeRepr], ret: TypeRepr) = EClass(defn.FunctionClass(tpes.length), tpes :+ ret)

  object EClass:
    def of(path: String, tpes: List[TypeRepr] = Nil): EClass =
      EClass(Symbol.requiredClass(path), tpes)

  class EClass(val cls: Symbol, tpes: List[TypeRepr]):
    lazy val ctorTypeRef = cls.typeRef
    lazy val typeRef = ctorTypeRef.appliedTo(tpes)

    def createInstance(params: List[Term]): Term =
      New(Inferred(ctorTypeRef))
        .select(cls.primaryConstructor)
        .appliedToTypes(tpes)
        .appliedToArgs(params)