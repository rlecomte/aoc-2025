case class Input(redTiles: List[(Long, Long)]) {

  val connections = redTiles.zip(redTiles.tail ++ List(redTiles.head))

  def isOnEdge(col: Long, row: Long): Boolean =
    connections.exists { case ((a, b), (c, d)) =>
      col >= Math.min(a, c) && col <= Math.max(a, c) && row >= Math.min(b, d) && row <= Math.max(b, d)
    }

  def isInside(col: Long, row: Long) =
    connections.filter { case ((x1, y1), (x2, y2)) =>
      x1 == x2 && col < x1 && row >= Math.min(y1, y2) && row < Math.max(y1, y2)
    }.size % 2 != 0

  val verticals = connections.filter { case ((x1, y1), (x2, y2)) => x1 == x2 }
  val horizontals = connections.filter { case ((x1, y1), (x2, y2)) => y1 == y2 }
  def intersect(colMin: Long, colMax: Long, rowMin: Long, rowMax: Long): Boolean = {
    val verticalCheck = verticals.exists { case ((x, y1), (_, y2)) =>
        if (x > colMin && x < colMax) {
          val overlapMin = Math.max(rowMin, y1)
          val overlapMax = Math.min(rowMax, y2)
          overlapMin < overlapMax
        } else false
    }

    val horizontalCheck = horizontals.exists { case ((x1, y), (x2, _)) =>
       if (y > rowMin && y < rowMax) {
         val overlapMin = Math.max(colMin, x1)
         val overlapMax = Math.min(colMax, x2)
         overlapMin < overlapMax
       } else false
    }

    verticalCheck || horizontalCheck
  }

  def validPosition(col: Long, row: Long): Boolean =
    isOnEdge(col, row) || isInside(col, row)
}

class Compute(input: Input):
  def compute =
    val r = (for
      (a, b)   <- input.redTiles
      (c, d)   <- input.redTiles
      if input.validPosition(a, d) && input.validPosition(c, b) && !input.intersect(Math.min(a, c), Math.max(a, c), Math.min(b, d), Math.max(b, d))
    yield ((a, b), (c, d)) -> (Math.abs(a - c) + 1) * (Math.abs(b - d) + 1)).maxBy(_._2)
    println(r)
    r

println(s"result: \n ${new Compute(Input(input)).compute}")

def realInput = Input(input)
def input = parse(scala.io.Source.fromFile("day9/day9.txt").mkString)

def parse(input: String): List[(Long, Long)] = input.trim.split("\n").map { s =>
  val arr = s.split(",")
  (arr.head.toLong, arr.tail.head.toLong)
}.toList
