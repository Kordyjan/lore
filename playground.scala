//> using scala "3.nightly"
///> using options "--explain"

import lore.*
import app.*

def work = taskTransform {
  log(download)
  27
}

@main def playground: Unit =
  given Log = app.Terminal
  given DB = app.DBMock
  given Int = 8

  val x = work.run
  println(x)