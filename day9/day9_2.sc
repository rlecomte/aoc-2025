case class Input(redTiles: List[(Long, Long)]) {
  val connections = redTiles.zip(redTiles.tail ++ List(redTiles.head))

  def isInside(col: Long, row: Long) =
    connections.filter { case ((x1, y1), (x2, y2)) =>
      x1 == x2 && col < x1 && row >= Math.min(y1, y2) && row < Math.max(y1, y2)
    }.size % 2 != 0

  def valid(colMin: Long, colMax: Long, rowMin: Long, rowMax: Long): Boolean =
    connections.forall { case ((x1, y1), (x2, y2)) =>
      if (x1 == x2) {
        x1 <= colMin ||
          colMax <= x1 ||
          y1.max(y2) <= rowMin ||
          y1.min(y2) >= rowMax
      } else {
        y1 <= rowMin ||
          rowMax <= y1 ||
          x1.max(x2) <= colMin ||
          x1.min(x2) >= colMax
      }
    }
}

class Compute(input: Input):
  case class Rectangle(colMin: Long, colMax: Long, rowMin: Long, rowMax: Long):
    val area: Long = (colMax - colMin + 1) * (rowMax - rowMin + 1)

  def rectangles = (for
      ((a, b), i) <- input.redTiles.zipWithIndex
      (c, d)      <- input.redTiles.drop(i + 1)
      colMin = Math.min(a, c)
      colMax = Math.max(a, c)
      rowMin = Math.min(b, d)
      rowMax = Math.max(b, d)
      middleCol = colMin + ((colMax - colMin) / 2)
      middleRow = rowMin + ((rowMax - rowMin) / 2)
      if input.isInside(middleCol, middleRow)
  yield Rectangle(colMin, colMax, rowMin, rowMax)).sortBy(-_.area)

  def compute = rectangles.find(r => input.valid(r.colMin, r.colMax, r.rowMin, r.rowMax)).get.area

println(s"result: \n ${new Compute(Input(input)).compute}")

def realInput = Input(input)
def input = parse(scala.io.Source.fromFile("day9/day9.txt").mkString)

def parse(input: String): List[(Long, Long)] = input.trim.split("\n").map { s =>
  val arr = s.split(",")
  (arr.head.toLong, arr.tail.head.toLong)
}.toList
