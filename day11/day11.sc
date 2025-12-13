def compute(graph: Map[String, Set[String]]) =
  var count = 0
  var nexts = List("you")

  while (nexts.nonEmpty)
    val node = nexts.head
    nexts = nexts.tail

    val nodes = graph.getOrElse(node, Set.empty)
    if (nodes.contains("out")) count = count + 1
    nexts = (nodes - "out").toList ++ nexts

  count

println(s"result: ${compute(dataset)}")

def dataset = parse(scala.io.Source.fromFile("day11/day11.txt").mkString)

def parse(input: String) = input.split("\n").foldLeft(Map.empty[String, Set[String]]) { case (acc, row) =>
  val from = row.split(":").head.trim
  val to = row.split(":").tail.head.trim.split(" ").map(_.trim).toSet

  acc.get(from) match
    case Some(set) => acc + (from -> (set ++ to))
    case None => acc + (from -> to)
}
