case class Input(freshIngredientIds: Set[(Long, Long)], ingredient: Set[Long])
def compute(input: Input) =
  input.ingredient.filter(id => input.freshIngredientIds.exists((a, b) => a <= id && id <= b)).size

println(s"result: ${compute(input)}")

def input = parse(scala.io.Source.fromFile("day5/day5.txt").mkString)

def parse(input: String) =
  val rows = input.trim.split("\n")
  val ranges = rows.takeWhile(_.nonEmpty)
  val ingredients = rows.dropWhile(_.nonEmpty).tail

  def parseRange(s: String) =
    val (x, y) = s.splitAt(s.indexOf('-'))
    (x.toLong, y.tail.toLong)

  Input(ranges.map(parseRange).toSet, ingredients.map(_.toLong).toSet)
