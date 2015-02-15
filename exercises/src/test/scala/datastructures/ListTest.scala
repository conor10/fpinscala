package datastructures

import org.scalatest.FunSuite

import fpinscala.datastructures.List
import fpinscala.datastructures.List._

/**
 * List unit tests.
 */
class ListTest extends FunSuite {

  test("append") {
    println(List(1, 2, 3, 4))
    assert(List(1, 2, 3, 4) === append(List(1, 2), List(3, 4)))
  }

  test("tail") {
    assert(List(2, 3, 4) === tail(List(1, 2, 3, 4)))
  }

  test("setHead") {
    assert(List(1, 2, 3) === setHead(List(2, 3), 1))
  }

  test("drop") {
    assert(List(3, 4) === drop(List(1, 2, 3, 4), 2))
  }

  test("dropWhile") {
    assert(List(3, 4) === dropWhile(List(1, 2, 3, 4), (x: Int) => x < 3))
  }

  test("length") {
    assert(4 === length(List(1, 2, 3, 4)))
  }

  test("sum2L") {
    assert(15 === sum2L(List(1, 2, 3, 4, 5)))
  }

  test("product2L") {
    assert(120 === product2L(List(1, 2, 3, 4, 5)))
  }

  test("lengthL") {
    assert(4 === lengthL(List(1, 2, 3, 4)))
  }

  test("reverse") {
    assert(List(4, 3, 2, 1) === reverse(List(1, 2, 3, 4)))
  }

  test("foldLeftWithFoldRight") {
    assert(10 === foldLeftWithFoldRight(List(1, 2, 3, 4), 0)(_ + _))
  }

  test("foldAppend") {
    assert(List(1, 2, 3, 4) === foldAppend(List(1, 2), List(3, 4)))
  }

  test("addOne") {
    assert(List(2, 3, 4) === addOne(List(1, 2, 3)))
  }

  test("convert") {
    assert(List(1.0.toString, 2.0.toString, 3.0.toString)
      === convert(List(1.0, 2.0, 3.0)))
  }

  test("map") {
    assert(List(2, 4, 6) === map(List(1, 2, 3))(_ * 2))
  }

  test("filter") {
    assert(List(2, 4) === filter(List(1, 2, 3, 4, 5))(_ % 2 == 0))
  }

  test("flatMap") {
    assert(List(1, 1, 2, 2, 3, 3) === flatMap(List(1, 2, 3))(i => List(i, i)))
  }

  test("filterFlatMap") {
    assert(List(2, 4) === filterFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 0))
  }

  test("add") {
    assert(List(5, 7, 9) === add(List(1, 2, 3), List(4, 5, 6)))
  }

  test("zipWith") {
    assert(List(5, 7, 9) ===
      zipWith(List(1, 2, 3), List(4, 5, 6))((x, y) => x + y))
  }
}
