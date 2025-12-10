//> using test.dep org.scalameta::munit::1.0.2
//> using file day9_2.sc

import day9_2.*

class Day9Tests extends munit.FunSuite {
  test("Check position inside polygon") {
    assert(Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (1, 3)
    )).validPosition(2, 2)) //inside

    assert(Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (1, 3)
    )).validPosition(3, 2)) //inside

    assert(Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (1, 3)
    )).validPosition(2, 1)) //inside on edge

    assert(Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (1, 3)
    )).validPosition(2, 3)) //inside on edge

    assert(Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (1, 3)
    )).validPosition(1, 2)) //inside on edge

    assert(Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (1, 3)
    )).validPosition(3, 2)) //inside on edge

    assert(!Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (1, 3)
    )).validPosition(4, 2)) //outside

    assert(!Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (1, 3)
    )).validPosition(2, 4)) //outside

    assert(!Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (1, 3)
    )).validPosition(0, 0)) //outside

    assert(!Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (1, 3)
    )).validPosition(4, 3)) //outside edge

    assert(!Input(List(
      (1, 1),
      (3, 1),
      (3, 3),
      (1, 3)
    )).validPosition(4, 4)) //outside

    assert(Input(List(
      (7,1),
      (11,1),
      (11,7),
      (9,7),
      (9,5),
      (2,5),
      (2,3),
      (7,3),
    )).validPosition(9, 3))

    val input = Input(List(
      (7,1),
      (11,1),
      (11,7),
      (9,7),
      (9,5),
      (2,5),
      (2,3),
      (7,3),
    ))
    //assert(!input.isInside(0, 0))

    //51661,1595
    //52890,1595
    assert(!realInput.isInside(52892, 1595))
    assert(realInput.isInside(52889, 1595))

    //52890,1595
    //52890,1870
    assert(!realInput.isInside(52890, 1594))
    assert(realInput.isOnEdge(52890, 1595))
  }
}
