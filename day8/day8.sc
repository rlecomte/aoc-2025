case class Pos(x: Long, y: Long, z: Long)

def compute(positions: List[Pos]) =
  val circuits = buildCircuits(distances(positions))
  circuits match
    case x :: x2 :: x3 :: _ =>
      x * x2 * x3
    case _ => throw new IllegalStateException()


def buildCircuits(edges: List[(Pos, Pos)]): List[Int] =
  edges.foldLeft(Set.empty[Set[Pos]]) { case (circuits, (a, b)) =>
    val circuitA = circuits.find(_.contains(a))
    val circuitB = circuits.find(_.contains(b))

    (circuitA, circuitB) match
      case (None, None) => circuits + Set(a, b)
      case (Some(circuit), None) => (circuits - circuit) + (circuit ++ Set(b))
      case (None, Some(circuit)) => (circuits - circuit) + (circuit ++ Set(a))
      case (Some(circuitA), Some(circuitB)) => (circuits - circuitB - circuitA) + (circuitA ++ circuitB)
  }.toList.map(_.size).sorted.reverse


def distances(positions: List[Pos]) = (for {
  (a, i) <- positions.zipWithIndex
  b      <- positions.drop(i + 1)
} yield a -> b).sortBy(euclidianDistance(_, _)).take(1000)


def euclidianDistance(a: Pos, b: Pos): Double = Math.sqrt(
  ((a.x - b.x) * (a.x - b.x)) + ((a.y - b.y) * (a.y - b.y)) + ((a.z - b.z) * (a.z - b.z))
)

def minEuclidianDistance(a: Pos, others: Set[Pos]): Pos = others.minBy(euclidianDistance(a, _))

println(s"result: ${compute(input)}")

def input = parse(scala.io.Source.fromFile("day8/day8.txt").mkString)

def parse(input: String) = input.split("\n").map(_.trim.split(",").map(_.trim.toLong).toList).map {
  case x :: y :: z :: Nil => Pos(x, y, z)
  case _ => throw IllegalArgumentException()
}.toList
