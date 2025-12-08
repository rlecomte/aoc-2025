import scala.util.Try
def compute(input: Vector[Vector[Boolean]]) = input.zipWithIndex.foldLeft(0) { case (acc, (row, x)) =>
  row.zipWithIndex.foldLeft(acc) {
    case (acc, (true, y)) if rollOfPaperIsAccessible(x, y, input) =>
      println(s"$x, $y is accessible")
      acc + 1
    case (acc, _) => acc
  }
}

println(s"result: ${compute(input)}")

def rollOfPaperIsAccessible(x: Int, y: Int, input: Vector[Vector[Boolean]]): Boolean =
  val results = List (
    (x - 1, y - 1), (x - 1, y), (x - 1, y + 1),
    (x, y - 1), (x, y + 1),
    (x + 1, y - 1), (x + 1, y), (x + 1, y + 1),
  ).filter((x, y) => Try(input(x)(y)).getOrElse(false)).size

  results < 4


def input = parse(scala.io.Source.fromFile("day4/day4.txt").mkString)

def parse(input: String) = input.trim.split("\n").map(_.trim.toVector.map(_ == '@')).toVector
