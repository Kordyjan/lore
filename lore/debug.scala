package lore

import quoted.*
import scala.annotation.targetName

def bebug(using Quotes)(arg: Seq[quotes.reflect.TypeRepr]): Unit =
  arg.foreach(bebug(_))

def bebug(using Quotes)(arg: quotes.reflect.TypeRepr): Unit =
  println("***")
  println(arg)
  println("<->")
  println(arg.show)

@targetName("bebugTree") def bebug(using Quotes)(arg: quotes.reflect.Tree): Unit =
  println("***")
  println(arg)
  println("<->")
  println(arg.show)
