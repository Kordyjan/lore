package app

trait DB:
  def get(id: Int): String

trait Log:
  def put(msg: String): Unit

def download(using DB): String = summon[DB].get(37)
def log(msg: String)(using Log) = summon[Log].put(msg)

object DBMock extends DB:
  def get(id: Int) = "0x%04x".formatted(id)

object Terminal extends Log:
  def put(msg: String): Unit = println(msg)