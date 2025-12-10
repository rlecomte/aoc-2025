//> using test.dep org.scalameta::munit::1.0.2
//> using file day9_2.sc

import day9_2.*

class Day9Tests extends munit.FunSuite {
  test("Work with vertical dents") {
    assert(new Compute(Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (5, 3),
      (5, 1),
      (7, 1),
      (7, 5),
      (1, 5)
    ))).compute._2 == 15)
  }

  test("Work with horizontal dents") {
    assert(new Compute(Input(List(
      (1, 1),
      (5, 1),
      (5, 3),
      (3, 3),
      (3, 4),
      (5, 4),
      (5, 6),
      (1, 6)
    ))).compute._2 == 15)
  }
}
