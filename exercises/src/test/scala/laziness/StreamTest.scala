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

  // TODO: See if we can avoid using toList here
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
}
