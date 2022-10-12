package lore

import quoted.*
import language.experimental.fewerBraces

def extend(using q: Quotes) = QuotesUtils[q.type]

class QuotesUtils[Q <: Quotes](using val q: Q):
  import q.reflect.*

  def varargs(tpe: TypeRepr)(values: List[Term]): Term =
    Typed(
      Repeated(values, Inferred(tpe)),
      Inferred(EClass(defn.RepeatedParamClass, tpe :: Nil).typeRef)
    )

  extension (term: Term)

    def cast(tpe: TypeRepr): Term =
      val method = defn.AnyClass.methodMember("asInstanceOf").head
      term.select(method).appliedToType(tpe)

  object ETuple:
    def fill(size: Int)(tpe: => TypeRepr) = of(List.fill(size)(tpe))
    def ofAny(size: Int) = fill(size)(defn.AnyClass.typeRef)
    def of(tpes: List[TypeRepr]) = EClass(defn.TupleClass(tpes.length), tpes)

  object EFunction:
    def of(tpes: List[TypeRepr], ret: TypeRepr) =
      EClass(defn.FunctionClass(tpes.length), tpes :+ ret)

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

  class EList(tpe: TypeRepr):
    val cls = Symbol.requiredClass("scala.collection.immutable.List")
    val companion = cls.companionModule
    lazy val ctorTypeRef = cls.typeRef
    lazy val typeRef = ctorTypeRef.appliedTo(tpe)

    def createInstance(params: List[Term]): Term =
      Ref(companion)
        .select(companion.methodMember("apply").head)
        .appliedToType(tpe)
        .appliedTo(varargs(tpe)(params))

  class EMap(keyTpe: TypeRepr, valTpe: TypeRepr):
    val cls = Symbol.requiredClass("scala.collection.immutable.Map")
    val companion = cls.companionModule
    lazy val ctorTypeRef = cls.typeRef
    lazy val typeRef = ctorTypeRef.appliedTo(keyTpe :: valTpe :: Nil)

    def createInstance(params: List[(Term, Term)]): Term =
      val tuple = ETuple.of(keyTpe :: valTpe :: Nil)
      val pairs = params.map: (k, v) =>
        tuple.createInstance(k :: v :: Nil)

      Ref(companion)
        .select(companion.methodMember("apply").head)
        .appliedToTypes(keyTpe :: valTpe :: Nil)
        .appliedTo(varargs(tuple.typeRef)(pairs))
