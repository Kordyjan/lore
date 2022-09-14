//> using scala "3.2.2-RC1-bin-20220909-eaa2889-NIGHTLY"

import lore.task
import app.*

def work(using DB, Log) = task {
  log(download)
  27
}

@main def playground(): Unit =
  given Log = app.Terminal
  given DB = app.DBMock

  val x: Int = work
  println(x)
