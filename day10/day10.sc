case class Input(lights: Vector[Boolean], button: Vector[Set[Int]]) {
  val intButtons = button.map(_.map(i => 1 << i).reduce(_ | _))

  val intLigths = lights.zipWithIndex.map((b, i) => if b then 1 << i else 0).reduce(_ | _)
}

def compute(inputs: List[Input]) = inputs.map(solve).sum

def solve(input: Input): Int = {
  var solved = false
  var depth = 1
  var state = input.intButtons

  while (!solved)
    if (state.contains(input.intLigths)) then solved = true
    else
      depth = depth + 1
      state = for
        s <- state
        b <- input.intButtons
      yield s ^ b

  depth
}

println(s"result: ${compute(dataset)}")

def dataset = parse(scala.io.Source.fromFile("day10/day10.txt").mkString)

def parse(input: String) = (for
  row <- input.split("\n")
  lights = """\[([\.\#]+)\]""".r.findFirstMatchIn(row).get.group(1).map( {
    case '.' => false
    case '#' => true
  }).toVector
  button = """\(([\d+,]+)\)""".r.findAllMatchIn(row).map(_.group(1)).map(s => s.split(",").map(_.toInt).toSet).toVector
yield Input(lights, button)).toList
