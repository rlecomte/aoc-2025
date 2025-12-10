def compute(redTiles: List[(Long, Long)]) =
  val maxX = redTiles.map(_._1).max
  val minX = redTiles.map(_._1).min
  val maxY = redTiles.map(_._2).max
  val minY = redTiles.map(_._2).min

  val topLeft = redTiles.sortBy((x, y) => x - minX + y - minY)
  val topRight = redTiles.sortBy((x, y) => maxX - x + y - minY)
  val bottomLeft = redTiles.sortBy((x, y) => x - minX + maxY - y)
  val bottomRight = redTiles.sortBy((x, y) => maxX - x + maxY - y)

  def lowerColOnSameRow(col: Long, row: Long) = redTiles.exists((x, y) => x <= col && y == row)
  def upperColOnSameRow(col: Long, row: Long) = redTiles.exists((x, y) => x >= col && y == row)
  def lowerRowOnSameCol(col: Long, row: Long) = redTiles.exists((x, y) => x == col && y <= row)
  def upperRowOnSameCol(col: Long, row: Long) = redTiles.exists((x, y) => x == col && y >= row)

  val rec1 = (for
    (a, b)   <- redTiles
    (c, d)   <- redTiles
    if a >= c && b >= d
    if (upperColOnSameRow(a, d) || lowerRowOnSameCol(a, d)) && (lowerColOnSameRow(c, b) || upperRowOnSameCol(c, b))
    //_ = println(s"($a, $b) / ($c, $d)")
  yield (a - c + 1) * (b - d + 1)).max

  val rec2 = (for
    (a, b)   <- redTiles
    (c, d)   <- redTiles
    if a >= c && b <= d
    if (lowerColOnSameRow(c, b) || lowerRowOnSameCol(c, b)) && (upperColOnSameRow(a, d) || upperRowOnSameCol(a, d))
    //_ = println(s"($a, $b) / ($c, $d)")
  yield (a - c + 1) * (d - b + 1)).max


  //val rec1 = (bottomRight._1 - topLeft._1 + 1) * (bottomRight._2 - topLeft._2 + 1)
  //val rec2 = (topRight._1 - bottomLeft._1 + 1) * (bottomLeft._2 - topRight._2 + 1)
  Math.max(rec1, rec2)

println(s"result: ${compute(input)}")

def input = parse(scala.io.Source.fromFile("day9/day9-test.txt").mkString)

def parse(input: String): List[(Long, Long)] = input.trim.split("\n").map { s =>
  val arr = s.split(",")
  (arr.head.toLong, arr.tail.head.toLong)
}.toList
