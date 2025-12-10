case class Input(redTiles: List[(Long, Long)]) {

  val redTilesSet = redTiles.toSet

  val greenTiles = redTiles.zip(redTiles.tail).flatMap { case ((a, b), (c, d)) =>
    if (a == c) List.range(b + 1, d).map(a -> _)
    else List.range(a + 1, c).map(_ -> b)
  }.toList

  def coverOnTopLeft(col: Long, row: Long) = greenTiles.exists((x, y) => col > x && row > y)
  def coverOnTopRight(col: Long, row: Long) = greenTiles.exists((x, y) => col < x && row > y)
  def coverOnBottomLeft(col: Long, row: Long) = greenTiles.exists((x, y) => col > x && row < y)
  def coverOnBottomRight(col: Long, row: Long) = greenTiles.exists((x, y) => col < x && row < y)

  def validPosition(col: Long, row: Long) =
    println(s"is valid? $col / $row")
    println(s"is valid on coverOnTopLeft? ${coverOnTopLeft(col, row)}")
    println(s"is valid on coverOnTopRight? ${coverOnTopRight(col, row)}")
    println(s"is valid on coverOnBottomLeft? ${coverOnBottomLeft(col, row)}")
    println(s"is valid on coverOnBottomRight? ${coverOnBottomRight(col, row)}")
    redTiles.contains(col -> row) || greenTiles.contains(col -> row) || coverOnTopLeft(col, row) || coverOnTopRight(col, row) || coverOnBottomLeft(col, row) || coverOnBottomRight(col, row)
}

class Compute(input: Input):
  def compute =
    (for
      (a, b)   <- input.redTiles // bottom right
      (c, d)   <- input.redTiles // topLeft
      //_ = println(s"($a, $b) / ($c, $d)")
    yield (Math.abs(a - c) + 1) * (Math.abs(b - d) + 1)).max

  def isValidRectangle(a: Long, b: Long, c: Long, d: Long) =
        if (a >= c && b >= d) input.validPosition(a, d) && input.validPosition(c, b)
        else if (a >= c && b <= d) input.validPosition(c, b) && input.validPosition(a, d)
        else if (a <= c && b >= d) input.validPosition(a, d) && input.validPosition(c, b)
        else input.validPosition(c, b) && input.validPosition(a, d)

//println(s"result: ${new Compute(Input(input)).compute}")

def input = parse(scala.io.Source.fromFile("day9/day9-test.txt").mkString)

def parse(input: String): List[(Long, Long)] = input.trim.split("\n").map { s =>
  val arr = s.split(",")
  (arr.head.toLong, arr.tail.head.toLong)
}.toList
