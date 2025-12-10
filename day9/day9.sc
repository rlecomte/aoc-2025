def compute(redTiles: List[(Long, Long)]) =
  val maxX = redTiles.map(_._1).max
  val minX = redTiles.map(_._1).min
  val maxY = redTiles.map(_._2).max
  val minY = redTiles.map(_._2).min

  val topLeft = redTiles.minBy((x, y) => x - minX + y - minY)
  val topRight = redTiles.minBy((x, y) => maxX - x + y - minY)
  val bottomLeft = redTiles.minBy((x, y) => x - minX + maxY - y)
  val bottomRight = redTiles.minBy((x, y) => maxX - x + maxY - y)

  val rec1 = (bottomRight._1 - topLeft._1 + 1) * (bottomRight._2 - topLeft._2 + 1)
  val rec2 = (topRight._1 - bottomLeft._1 + 1) * (bottomLeft._2 - topRight._2 + 1)
  Math.max(rec1, rec2)

println(s"result: ${compute(input)}")

def input = parse(scala.io.Source.fromFile("day9/day9.txt").mkString)

def parse(input: String): List[(Long, Long)] = input.trim.split("\n").map { s =>
  val arr = s.split(",")
  (arr.head.toLong, arr.tail.head.toLong)
}.toList
