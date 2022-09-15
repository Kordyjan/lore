//> using scala "3.2.2-RC1-bin-20220909-eaa2889-NIGHTLY"

import lore.*
import app.*

def work = task {
  log(download)
  27
}

@main def playground(): Unit =
  given Log = app.Terminal
  given DB = app.DBMock

  val x: Int = work((summon[Log], summon[DB]))
  println(x)
