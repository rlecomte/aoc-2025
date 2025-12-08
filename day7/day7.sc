enum Elem { case Blank, S, Splitter }
case class Input(rows: Vector[Vector[Elem]]) {
  val numRows = rows.size
  val indexS    = rows.head.indexOf(Elem.S)
}

def compute(input: Input) = go(input, 0, input.indexS, Set.empty).size

def go(input: Input, row: Int, col: Int, breadcrumbs: Set[(Int, Int)]): Set[(Int, Int)] =
  if (row < input.numRows && !breadcrumbs.contains((row, col)))
    input.rows(row)(col) match
      case Elem.Splitter =>
         val updatedBreadcrumbs = go(input, row + 1, col - 1, Set((row, col)) ++ breadcrumbs)
         go(input, row + 1, col + 1, updatedBreadcrumbs)
      case _ => go(input, row + 1, col, breadcrumbs)
  else breadcrumbs


println(s"result: ${compute(input)}")

def input = parse(scala.io.Source.fromFile("day7/day7.txt").mkString)

def parse(input: String) =
  val rows = input.split("\n")
  val elems = rows.map(_.trim).map(_.map {
    case 'S'  => Elem.S
    case '.'  => Elem.Blank
    case '^'  => Elem.Splitter
    case s => throw new IllegalArgumentException(s.toString())
  }.toVector).toVector
  Input(elems)
