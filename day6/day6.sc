enum Ops { case Mul, Add }
case class Input(ops: Vector[Ops], numbers: Vector[Vector[Long]]) {
  val numRow = numbers.size
  val numCol = numbers.head.size

  def getColumn(colIdx: Int) = (List.range(0, numRow).map(numbers(_)(colIdx)), ops(colIdx))
}

def compute(input: Input) = List.range(0, input.numCol).foldLeft(0L) { case (acc, colIdx) =>
  val (columnValues, op) = input.getColumn(colIdx)
  val result = op match
    case Ops.Add => columnValues.sum
    case Ops.Mul => columnValues.foldLeft(1L)((acc, v) => acc * v)
  acc + result
}

println(s"result: ${compute(input)}")

def input = parse(scala.io.Source.fromFile("day6/day6.txt").mkString)

def parse(input: String) =
 val rows = input.trim.split("\n")
 val numbers = rows.init.map(_.split(" ").filter(_.trim.nonEmpty).map(_.trim.toLong).toVector).toVector
 val ops = rows.last.split(" ").filter(_.trim.nonEmpty).map(_.trim).map {
   case "+" => Ops.Add
   case "*" => Ops.Mul
 }.toVector

 Input(ops, numbers)
