def compute(moves: List[Move]) = moves.scanLeft(50 -> 0) {
  case ((acc, result), Move.Left(unbound))  =>
    val n = unbound % 100
    if acc == n then (0, result + 1)
    else if acc < n then (100 - (n - acc), result)
    else (Math.abs(n - acc), result)
  case ((acc, result), Move.Right(unbound)) =>
    val n = unbound % 100
    if acc + n == 100 then (0, result + 1)
    else if acc + n > 99 then (n - (100 - acc), result)
    else (n + acc, result)
}.last

println(s"result: ${compute(input)._2}")

enum Move:
  case Left(unbound: Int)
  case Right(unbound: Int)

object Move:
  def fromInput(v: String): Move = v.head match
    case 'L' => Move.Left(v.tail.toInt)
    case 'R' => Move.Right(v.tail.toInt)

def input = scala.io.Source.fromFile("day1/day1.txt").mkString
  .split("\n").toList.map(_.trim).filter(_.nonEmpty).map(Move.fromInput)

def inputTest = """
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
""".split("\n").toList.map(_.trim).filter(_.nonEmpty).map(Move.fromInput)

