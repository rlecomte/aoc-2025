//> using test.dep org.scalameta::munit::1.0.2
//> using file day3.sc

import day3.*

class Day3Tests extends munit.FunSuite {
  test("11-22") {
    assertEquals(compute(List(11L -> 22L)), Set(11L, 22L))
  }
}
