package jem

import io.Source

object MyBot extends Application {
  def parseTurnState(lines: Iterator[String]) = {
    def parse(strings: List[String]): Seq[Planet] = {
      val line = lines.next
      if ("go".equals(line)) Parser.parse(strings) else parse(line :: strings)
    }
    parse(Nil)
  }
  val bot = new PassTheParcelBot()
  val source = Source.fromInputStream(System.in)
  try {
    while (true) {
      val turnState = parseTurnState(source.getLines)
      val orders = bot.respondTo(turnState)

/*
      orders.map(o => o.inServerSpeak + " (%s %d) -> (%s %d) %d".format(o.from.owner, o.from.size, o.to.owner, o.to.size, o.quantity)).foreach(Log.debug)
      Log.debug("go")
*/

      orders.map(_.inServerSpeak).foreach(println)
      println("go")
    }
  } catch {
    case t => Log.debug(t)
  }
}