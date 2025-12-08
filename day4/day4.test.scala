//> using test.dep org.scalameta::munit::1.0.2
//> using file day4.sc

import day4.*

class Day4Tests extends munit.FunSuite {
  test("center") {
    assertEquals(compute(parse(
      """
        ...
        .@.
        ...
      """
    )), 1)
    assertEquals(compute(parse(
      """
        @.@
        ...
        @.@
      """
    )), 4)
 }
}
