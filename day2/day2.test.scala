//> using test.dep org.scalameta::munit::1.0.2
//> using file day2.sc

import day2.*

class Day2Tests extends munit.FunSuite {
  test("11-22") {
    assertEquals(compute(List(11L -> 22L)), Set(11L, 22L))
  }
  test("1188511880-1188511890") {
    assertEquals(compute(List(1188511880L -> 1188511890L)), Set(1188511885L))
  }
}
