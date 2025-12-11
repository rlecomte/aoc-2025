import scala.io.AnsiColor
import scala.collection.mutable.{ArraySeq, PriorityQueue}

def input = scala.io.Source.fromFile("day9/day9.txt").mkString.split("\n")

final case class RedTile(x: Long, y: Long)

def calculateArea(p1: RedTile, p2: RedTile): Long = {
  val dx = p1.x - p2.x
  val dy = p1.y - p2.y
  (dx.abs + 1) * (dy.abs + 1)
}

def parseInput(input: Array[String]): Array[RedTile] = {
  input.map { line =>
    val parts = line.split(",").map(_.toLong)
    RedTile(parts(0), parts(1))
  }
}

def makePairs(points: Array[RedTile]): Array[(RedTile, RedTile, Long)] = {
  val pairs = for {
    i <- points.indices
    j <- i + 1 until points.length
  } yield (points(i), points(j), calculateArea(points(i), points(j)))

  pairs.toArray
}

def oneStars(input: Array[String]): Long = {
  makePairs(parseInput(input)).maxBy(_._3)._3

}

def printTheater(theater: ArraySeq[ArraySeq[Char]], p1: RedTile, p2: RedTile, p3: RedTile, p4: RedTile): Unit = {
  for {
    (col, x) <- theater.zipWithIndex
    row = col.zipWithIndex.map { (c, y) =>
      if ((p1.x == x && p1.y == y) || (p2.x == x && p2.y == y)) s"${AnsiColor.GREEN}$c${AnsiColor.RESET}"
      else if ((p3.x == x && p3.y == y) || (p4.x == x && p4.y == y)) s"${AnsiColor.RED}$c${AnsiColor.RESET}"
      else c
    }
  } yield println(row.mkString)
}

def fillLine(
    a: RedTile,
    b: RedTile,
    theater: ArraySeq[ArraySeq[Char]],
    xs: Array[Long],
    ys: Array[Long]
): Unit = {
  val x1Index = xs.indexOf(a.x)
  val x2Index = xs.indexOf(b.x)
  val y1Index = ys.indexOf(a.y)
  val y2Index = ys.indexOf(b.y)

  if (a.x == b.x) {
    val xIndex = x1Index
    val yStart = Math.min(y1Index, y2Index)
    val yEnd = Math.max(y1Index, y2Index)

    for (y <- yStart to yEnd) {
      theater(y)(xIndex) = '#'
    }
  } else if (a.y == b.y) {
    val yIndex = y1Index
    val xStart = Math.min(x1Index, x2Index)
    val xEnd = Math.max(x1Index, x2Index)

    for (x <- xStart to xEnd) {
      theater(yIndex)(x) = '#'
    }
  }
}

def fillMiddleOther(theater: ArraySeq[ArraySeq[Char]]) = for {
  x <- theater.indices
  y <- theater(x).indices
} yield {
  if (theater(x)(y) != '#') {
    var left = false
    var right = false
    var up = false
    var down = false

    for (i <- 0 until x) {
      if (theater(i)(y) == '#') up = true
    }

    for (i <- x + 1 until theater.length) {
      if (theater(i)(y) == '#') down = true
    }

    for (j <- 0 until y) {
      if (theater(x)(j) == '#') left = true
    }

    for (j <- y + 1 until theater(x).length) {
      if (theater(x)(j) == '#') right = true
    }

    if (left && right && up && down) {
      theater(x)(y) = '#'
    }
  }
}

def inFilled(
    a: RedTile,
    b: RedTile,
    theater: ArraySeq[ArraySeq[Char]],
    xs: Array[Long],
    ys: Array[Long]
): Boolean = {
  val x1Index = xs.indexOf(a.x)
  val x2Index = xs.indexOf(b.x)
  val y1Index = ys.indexOf(a.y)
  val y2Index = ys.indexOf(b.y)

  val xStart = Math.min(x1Index, x2Index)
  val xEnd = Math.max(x1Index, x2Index)
  val yStart = Math.min(y1Index, y2Index)
  val yEnd = Math.max(y1Index, y2Index)

  for {
    x <- xStart to xEnd
    y <- yStart to yEnd
  } {
    if (theater(y)(x) != '#') return false
  }

  true
}

def twoStars(input: Array[String]) = {
  val points = parseInput(input)

  val xs = points.map(_.x).distinct.sorted
  val ys = points.map(_.y).distinct.sorted

  val theater = ArraySeq.fill(xs.length + 1, ys.length + 1)('.')

  val pairs = makePairs(points.toArray)

  pairs.foreach { case (p1, p2, _) =>
    fillLine(p1, p2, theater, xs, ys)
  }

  fillMiddleOther(theater)

  printTheater(
    theater,
    RedTile(xs.indexOf(5404), ys.indexOf(67350)),
    RedTile(xs.indexOf(94858), ys.indexOf(50402)),
    RedTile(xs.indexOf(4315), ys.indexOf(32874)),
    RedTile(xs.indexOf(94858), ys.indexOf(50402))
  )
  //(((94858,50402),(4315,32874)),1587145776)
  //printTheater(theater, RedTile(xs.indexOf(4315), ys.indexOf(32874)), RedTile(xs.indexOf(94858), ys.indexOf(50402)))

  pairs
    .sortBy(-_._3)
    .find { case (p1, p2, _) =>
      inFilled(p1, p2, theater, xs, ys)
    }
    .get
}

println(twoStars(input))
