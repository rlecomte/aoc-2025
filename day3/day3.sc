def compute(input: List[String]) =
  input.map(findJoltage)

def findJoltage(bank: String) =
  val digits = bank
  .toCharArray
  .map(_.asDigit)
  .zipWithIndex

  val (max1, idx1) = digits.maxBy(_._1)
  val (max2, idx2) = if (idx1 < digits.length - 1) digits.drop(idx1 + 1).max else digits.init.maxBy(_._1)
  if (idx2 > idx1) then s"$max1$max2".toInt else s"$max2$max1".toInt

println(s"result: ${compute(input).sum}")

def input = scala.io.Source.fromFile("day3/day3.txt").mkString.trim.split("\n").toList
