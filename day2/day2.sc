def compute(ranges: List[(Long, Long)]): List[Long] =
  ranges.flatMap((a, b) => List.range(a, b + 1)).filter(checkInvalid)

private def checkInvalid(n: Long): Boolean =
  val s = n.toString
  val (a, b) = s.splitAt(s.length / 2)
  a == b

println(s"result: ${compute(input).sum}")

def input = scala.io.Source.fromFile("day2/day2.txt").mkString.trim
  .split(",").map { s =>
    val chunks = s.split("-")
    if (chunks.head.trim.startsWith("0") || chunks.last.trim.startsWith("0")) {
      throw new Exception(s"Leading zeros are not allowed: '$s'")
    }
    chunks.head.trim.toLong -> chunks.last.trim.toLong
  }.toList
