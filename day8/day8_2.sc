//> using file day8.sc
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
case class Pos(x: Long, y: Long, z: Long)

def compute(positions: List[Pos]) =
  val (a, b) = buildCircuits(positions.map(Set(_)).toSet, distances(positions))
  a.x * b.x

def buildCircuits(circuits: Set[Set[Pos]], edges: Vector[(Pos, Pos)]): (Pos, Pos) =
  val (a, b) = edges.head

  val circuitA = circuits.find(_.contains(a)).get
  val circuitB = circuits.find(_.contains(b)).get

  val updatedCircuits = (circuits - circuitB - circuitA) + (circuitA ++ circuitB)

  if updatedCircuits.size == 1 then (a, b) else buildCircuits(updatedCircuits, edges.tail)

def distances(positions: List[Pos]): Vector[(Pos, Pos)] =
  (for {
      (a, i) <- positions.toVector.zipWithIndex
      b      <- positions.drop(i + 1)
  } yield a -> b).sortBy(euclidianDistance(_, _))


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
