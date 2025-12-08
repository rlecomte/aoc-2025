//> using test.dep org.scalameta::munit::1.0.2
//> using file day1.sc

import day1.*

class Day1Tests extends munit.FunSuite {
  test("Simple left") {
    assertEquals(compute(List(Move.Left(3))), (47, 0))
  }
  test("Simple left zero") {
    assertEquals(compute(List(Move.Left(50))), (0, 1))
  }
  test("Simple left outbound") {
    assertEquals(compute(List(Move.Left(51))), (99, 0))
  }
  test("Simple left multi outbound") {
    assertEquals(compute(List(Move.Left(400))), (50, 0))
  }
  test("Simple right") {
    assertEquals(compute(List(Move.Right(3))), (53, 0))
  }
  test("Simple right zero") {
    assertEquals(compute(List(Move.Right(50))), (0, 1))
  }
  test("Simple right outbound") {
    assertEquals(compute(List(Move.Right(51))), (1, 0))
  }
  test("Simple left multi outbound") {
    assertEquals(compute(List(Move.Right(400))), (50, 0))
  }
}
