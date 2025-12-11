case class Input(redTiles: List[(Long, Long)]) {

  val connections = redTiles.zip(redTiles.tail ++ List(redTiles.head))
  val verticals = connections.filter { case ((x1, y1), (x2, y2)) => x1 == x2 }
  val horizontals = connections.filter { case ((x1, y1), (x2, y2)) => y1 == y2 }

  def isInside(col: Long, row: Long) =
    connections.filter { case ((x1, y1), (x2, y2)) =>
      x1 == x2 && col < x1 && row >= Math.min(y1, y2) && row < Math.max(y1, y2)
    }.size % 2 != 0

  def intersect(colMin: Long, colMax: Long, rowMin: Long, rowMax: Long): Boolean = {
    val verticalCheck = verticals.exists { case ((x, y1), (_, y2)) =>
        if (x > colMin && x < colMax) {
          val overlapMin = Math.max(rowMin, y1)
          val overlapMax = Math.min(rowMax, y2)
          (overlapMin < overlapMax) || rowMin == Math.min(y1, y2) && rowMax == Math.max(y1, y2)
        } else false
    }

    val horizontalCheck = horizontals.exists { case ((x1, y), (x2, _)) =>
       if (y > rowMin && y < rowMax) {
         val overlapMin = Math.max(colMin, x1)
         val overlapMax = Math.min(colMax, x2)
         (overlapMin < overlapMax) || colMin == Math.min(x1, x2) && colMax == Math.max(x1, x2)
       } else false
    }

    verticalCheck || horizontalCheck
  }
}

class Compute(input: Input):
  def compute =
    val r = (for
      (a, b)   <- input.redTiles
      (c, d)   <- input.redTiles
      colMin = Math.min(a, c)
      colMax = Math.max(a, c)
      rowMin = Math.min(b, d)
      rowMax = Math.max(b, d)
      middleCol = colMin + ((colMax - colMin) / 2)
      middleRow = rowMin + ((rowMax - rowMin) / 2)
      if (middleCol == colMin || middleRow == rowMin || input.isInside(middleCol, middleRow)) && !input.intersect(colMin, colMax, rowMin, rowMax)
    yield ((a, b), (c, d)) -> (colMax - colMin + 1) * (rowMax - rowMin + 1)).maxBy(_._2)
    r

println(s"result: \n ${new Compute(Input(input)).compute}")

def realInput = Input(input)
def input = parse(scala.io.Source.fromFile("day9/day9.txt").mkString)

def parse(input: String): List[(Long, Long)] = input.trim.split("\n").map { s =>
  val arr = s.split(",")
  (arr.head.toLong, arr.tail.head.toLong)
}.toList
