package laziness

import org.scalatest.FunSuite

import fpinscala.laziness._
import fpinscala.laziness.Stream._

/**
 * Stream unit tests.
 */
class StreamTest extends FunSuite {
  test("toList") {
    assert(Nil === empty.toList)
    assert(List(1, 2, 3) === Stream(1, 2, 3).toList)
  }

  test("take") {
    assert(Empty === Stream(1, 2, 3).take(0))
    assert(List(1) === Stream(1, 2, 3).take(1).toList)
    assert(List(1, 2, 3) === Stream(1, 2, 3).take(3).toList)
  }

  test("drop") {
    assert(List(1, 2, 3) === Stream(1, 2, 3).drop(0).toList)
    assert(List(2, 3) === Stream(1, 2, 3).drop(1).toList)
    assert(Empty === Stream(1, 2, 3).drop(3))
  }

  test("takeWhile") {
    val p = (x: Int) => x < 4
    assert(List(1, 2, 3) === Stream(1, 2, 3, 4, 5).takeWhile(p).toList)
    assert(List(0, 1, 2, 3) === Stream(0, 1, 2, 3, 4).takeWhile(p).toList)
    assert(Empty === Stream(4, 1, 2, 3).takeWhile(p))
  }

  test("forAll") {
    val p = (x: Int) => x % 2 == 0
    assert(true === Empty.forAll(p))
    assert(true === Stream(0, 2, 4).forAll(p))
    assert(false === Stream(0, 1, 2).forAll(p))
  }

  test("takeWhileFold") {
    val p = (x: Int) => x < 4
    assert(List(1, 2, 3) === Stream(1, 2, 3, 4, 5).takeWhileFold(p).toList)
    assert(List(0, 1, 2, 3) === Stream(0, 1, 2, 3, 4).takeWhileFold(p).toList)
    assert(Empty === Stream(4, 1, 2, 3).takeWhileFold(p))
  }

  test("headOption") {
    assert(None === Empty.headOption)
    assert(Some(1) === Stream(1).headOption)
    assert(Some(1) === Stream(1, 2, 3).headOption)
  }

  test("map") {
    assert(List(2, 4, 6) === Stream(1, 2, 3).map((x: Int) => x *2).toList)
  }

  test("filter") {
    val p = (x: Int) => x % 2 == 0
    assert(Empty === Empty.filter(p))
    assert(List(2, 4) === Stream(1, 2, 3, 4).filter(p).toList)
    assert(Empty === Stream(1, 3).filter(p))
  }

  test("flatMap") {
    val f = (x: Int) => Stream(x, x)
    assert(Empty === Empty.flatMap(f))
    assert(List(1, 1, 2, 2, 3, 3) === Stream(1, 2, 3).flatMap(f).toList)
  }

  test("append") {
    assert(List(1, 2) === Stream(1, 2).append(Empty).toList)
    assert(List(1, 2, 3, 4) === Stream(1, 2).append(Stream(3, 4)).toList)
    assert(List(3, 4) === Empty.append(Stream(3, 4)).toList)
  }

  test("constant") {
    assert(List(1, 1, 1) === constant(1).take(3).toList)
  }

  test("from") {
    assert(List(1, 2, 3) === from(1).take(3).toList)
  }

  test("fibs") {
    assert(List(0, 1, 1, 2, 3, 5, 8) === fibs.take(7).toList)
  }

  test("onesUnfold") {
    assert(List(1, 1, 1) === onesUnfold.take(3).toList)
  }

  test("constantUnfold") {
    assert(List(1, 1, 1) === constantUnfold(1).take(3).toList)
  }

  test("fromUnfold") {
    assert(List(1, 2, 3) === fromUnfold(1).take(3).toList)
  }

  test("fibsUnfold") {
    assert(List(0, 1, 1, 2, 3, 5, 8) === fibsUnfold.take(7).toList)
  }

  test("mapUnfold") {
    assert(List(2, 4, 6) === Stream(1, 2, 3).mapUnfold((x: Int) => x*2).toList)
  }

  test("takeUnfold") {
    assert(Empty === Stream(1, 2, 3).takeUnfold(0))
    assert(List(1) === Stream(1, 2, 3).takeUnfold(1).toList)
    assert(List(1, 2, 3) === Stream(1, 2, 3).takeUnfold(3).toList)
  }

  test("takeWhileUnfold") {
    val p = (x: Int) => x < 4
    assert(List(1, 2, 3) === Stream(1, 2, 3, 4, 5).takeWhileUnfold(p).toList)
    assert(List(0, 1, 2, 3) === Stream(0, 1, 2, 3, 4).takeWhileUnfold(p).toList)
    assert(Empty === Stream(4, 1, 2, 3).takeWhileUnfold(p))
  }

  test("zipWith") {
    assert(List(5, 7, 9) ===
      Stream(1, 2, 3).zipWith(Stream(4, 5, 6))((x, y) => x + y).toList)
    assert(List(5, 7, 9) ===
      Stream(1, 2, 3, 4, 5).zipWith(Stream(4, 5, 6))((x, y) => x + y).toList)
  }

  test("zipAll") {
    assert(List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6)), (Some(4), None), (Some(5), None)) ===
      Stream(1, 2, 3, 4, 5).zipAll(Stream(4, 5, 6)).toList)
    assert(List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6)), (None, Some(7)), (None, Some(8))) ===
      Stream(1, 2, 3).zipAll(Stream(4, 5, 6, 7, 8)).toList)
  }

  test("startsWith") {
    assert(true === Stream(1, 2, 3).startsWith(Stream(1, 2)))
    assert(false === Stream(1, 2, 3).startsWith(Stream(2, 3)))
  }

  test("tails") {
    assert(List(List(1, 2, 3), List(2, 3), List(3), List()) === Stream(1, 2, 3).tails.toList.map(_.toList))
  }

  test("scanRight") {
    assert(List(6, 5, 3, 0) === Stream(1, 2, 3).scanRight(0)(_ + _).toList)
  }
}
