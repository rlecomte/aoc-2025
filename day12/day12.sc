import scala.util.Try
import scala.collection.mutable.ArrayBuffer

type Shape = Vector[Vector[Boolean]]

case class Faces(a: Shape) {
  val b = turnRight(a)
  val c = turnRight(b)
  val d = turnRight(c)

  def turnRight(s: Shape): Shape = {
    val arr = ArrayBuffer.fill(s.head.size)(ArrayBuffer.fill(s.size)(false))
    for {
      x <- 0 until s.size
      y <- 0 until s.head.size
    } yield arr(s.head.size - 1 - y)(x) = s(x)(y)
    arr.map(_.toVector).toVector
  }

  def space = a.size * a.head.size

  def allFaces = Set(a, b, c, d)
}

trait Mask {
  def get(x: Int, y: Int): Boolean
}

case class ShapeMask(shape: Shape, offsetX: Int, offsetY: Int, tail: Mask) extends Mask {
  override def get(x: Int, y: Int): Boolean =
    if (x >= offsetX && x < offsetX + shape.size && y >= offsetY && y < offsetY + shape.head.size) {
      shape(x - offsetX)(y - offsetY) || tail.get(x, y)
    } else tail.get(x, y)
}

object EmptyMask extends Mask {
  override def get(x: Int, y: Int): Boolean = false
}

case class Region(width: Int, height: Int, shapes: Vector[Int])

def compute(shapes: Vector[Faces], regions: Vector[Region]) = regions.filter(resolve(shapes, _)).size

def resolve(shapes: Vector[Faces], region: Region): Boolean =

  def spaceNeeded(remainingShapes: Vector[Int]): Int =
    (for {
      (faces, i) <- shapes.zipWithIndex
      if remainingShapes(i) > 0
      count      = remainingShapes(i)
    } yield count * faces.space).sum

  def go(mask: Mask, remainingShapes: Vector[Int], occupiedSpace: Int): Boolean =
    if (remainingShapes.exists(_ > 0)) {
      (for {
        (faces, i) <- LazyList.from(shapes.zipWithIndex)
        if remainingShapes(i) > 0
        updatedRemainingShapes = remainingShapes.updated(i, remainingShapes(i) - 1)
        updatedOccupiedSpace  = occupiedSpace + faces.space
        if spaceNeeded(updatedRemainingShapes) <=  region.width * region.height - updatedOccupiedSpace
        face       <- LazyList.from(faces.allFaces)
        subMask    <- LazyList.from(applyShape(mask, face, region.width, region.height))
        r          = go(subMask, updatedRemainingShapes, updatedOccupiedSpace)
      } yield r).exists(identity)
    } else {
      println("--------------------------------------------")
      println("")
      printshape(mask, region.width, region.height)
      true
    }

  var r = go(EmptyMask, region.shapes, 0)
  println(s"Region $region resolved: $r")
  r

def applyShape(mask: Mask, shape: Shape, width: Int, height: Int): LazyList[Mask] =
  def checkValidity(offsetX: Int, offestY: Int): Boolean =
    var x = 0
    var continue = true

    while (x < shape.size && continue)
      var y = 0
      while (y < shape.size && continue)
        if (x + offsetX < width && y + offestY < height )
          val maskValue = mask.get(x + offsetX, y + offestY)
          val s2Value = shape(x)(y)
          continue = !maskValue || !s2Value || maskValue ^ s2Value
        else continue = false
        y = y + 1
      x = x + 1

    continue

  for
    x <- LazyList.from(0 to width)
    y <- LazyList.from(0 to height)
    if checkValidity(x, y)
  yield ShapeMask(shape, x, y, mask)

def printshape(mask: Mask, width: Int, height: Int) =
  for
    y   <- 0 until height
    row = (0 until width).map(x => if mask.get(x, y) then "#" else ".").mkString
    _   = println(row)
  yield ()

println(s"result: ${compute.tupled(dataset)}")

def dataset = parse(scala.io.Source.fromFile("day12/day12.txt").mkString)

def parse(input: String) =
  def getShape(input: String): Shape = input.trim.split("\n").map(_.trim.toCharArray.map(_ == '#').toVector).toVector

  val shapes = """(\d+)\:\n([\.\#\n]+)""".r.findAllMatchIn(input).map(m => getShape(m.group(2))).toVector

  val regions = input.drop(input.lastIndexOf('#') + 1).trim.split("\n").map { region =>
    val m = """(\d+)x(\d+)\:(.+)""".r.findFirstMatchIn(region).get
    val x = m.group(1).toInt
    val y = m.group(2).toInt
    val shapesIndices = m.group(3).trim.split(" ").map(_.trim.toInt).toVector
    Region(x, y, shapesIndices)
  }.toVector

  (shapes.map(Faces(_)), regions)
