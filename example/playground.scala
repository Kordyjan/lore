//> using scala "3.nightly"
///> using options "--explain"
///> using options "-Xcheck-macros", "-Ycheck:all"

import language.experimental.fewerBraces

import lore.*
import app.*

def work = task:
  log(download)
  27

val work2: Int Using(DB & Log & Int) = work

@main def playground: Unit =
  given DB = app.DBMock
  given Log = app.Terminal
  given Int = 8

  work.run
  val x = work2.run
  println(x)