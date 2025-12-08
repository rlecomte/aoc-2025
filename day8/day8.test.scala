//> using test.dep org.scalameta::munit::1.0.2
//> using file day8.sc
//> using file day8_2.sc

import day8.*
import day8_2.{compute => compute2, Pos => Pos2}

class Day8Tests extends munit.FunSuite {

   val positions = List(
      Pos(162,817,812),
      Pos(57,618,57),
      Pos(906,360,560),
      Pos(592,479,940),
      Pos(352,342,300),
      Pos(466,668,158),
      Pos(542,29,236),
      Pos(431,825,988),
      Pos(739,650,466),
      Pos(52,470,668),
      Pos(216,146,977),
      Pos(819,987,18),
      Pos(117,168,530),
      Pos(805,96,715),
      Pos(346,949,466),
      Pos(970,615,88),
      Pos(941,993,340),
      Pos(862,61,35),
      Pos(984,92,344),
      Pos(425,690,689)
  )

  test("euclidian distance") {
    val result = distances(positions)
    assertEquals(result.take(4), List(
      Pos(162, 817, 812) -> Pos(425,690,689),
      Pos(162, 817, 812) -> Pos(431,825,988),
      Pos(906,360,560)   -> Pos(805,96,715),
      Pos(431,825,988)   -> Pos(425,690,689)
    ))
  }
  test("compute") {
    val result = compute(List(
      Pos(162,817,812),
      Pos(57,618,57),
      Pos(906,360,560),
      Pos(592,479,940),
      Pos(352,342,300),
      Pos(466,668,158),
      Pos(542,29,236),
      Pos(431,825,988),
      Pos(739,650,466),
      Pos(52,470,668),
      Pos(216,146,977),
      Pos(819,987,18),
      Pos(117,168,530),
      Pos(805,96,715),
      Pos(346,949,466),
      Pos(970,615,88),
      Pos(941,993,340),
      Pos(862,61,35),
      Pos(984,92,344),
      Pos(425,690,689)
    ))
    assertEquals(result, 40)
  }
  test("compute2") {
    val result = compute2(List(
      Pos2(162,817,812),
      Pos2(57,618,57),
      Pos2(906,360,560),
      Pos2(592,479,940),
      Pos2(352,342,300),
      Pos2(466,668,158),
      Pos2(542,29,236),
      Pos2(431,825,988),
      Pos2(739,650,466),
      Pos2(52,470,668),
      Pos2(216,146,977),
      Pos2(819,987,18),
      Pos2(117,168,530),
      Pos2(805,96,715),
      Pos2(346,949,466),
      Pos2(970,615,88),
      Pos2(941,993,340),
      Pos2(862,61,35),
      Pos2(984,92,344),
      Pos2(425,690,689)
    ))
    assertEquals(result, 25272L)
  }
}
