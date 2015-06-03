package fpinscala.errorhandling

import org.scalatest.FunSuite

import fpinscala.errorhandling.Option._

/**
 * Option unit tests.
 */
class OptionTest extends FunSuite {
  test("map") {
    val f = (x: Int) => x * 2
    assert(Some(1).map(f) === Some(2))
    assert(None.map(f) === None)
  }

  test("getOrElse") {
    assert(Some(1).getOrElse(2) === 1)
    assert(None.getOrElse(2) === 2)
  }

  test("flatMap") {
    val f = (x: Int) => Some(x * 2)
    assert(Some(1).flatMap(f) === Some(2))
    assert(None.flatMap(f) === None)
  }

  test("orElse") {
    assert(Some(1).orElse(Some(2)) === Some(1))
    assert(None.orElse(Some(2)) === Some(2))
  }

  test("orElseMatch") {
    assert(Some(1).orElseMatch(Some(2)) === Some(1))
    assert(None.orElseMatch(Some(2)) === Some(2))
  }

  test("filter") {
    val f = (x: Int) => x % 2 == 0
    assert(Some(1).filter(f) === None)
    assert(Some(2).filter(f) === Some(2))
  }

  test("variance") {
    assert(Some(2.0) === variance(List(1.0, 2.0, 3.0, 4.0, 5.0)))
  }

  test("map2") {
    val f = (x: Int, y: Int) => x + y
    assert(Some(3) === map2(Some(1), Some(2))(f))
    assert(None === map2(Some(1), None)(f))
    assert(None === map2(None, Some(2))(f))
    assert(None === map2(None, None)(f))
  }

  test("map2Match") {
    val f = (x: Int, y: Int) => x + y
    assert(Some(3) === map2Match(Some(1), Some(2))(f))
    assert(None === map2Match(Some(1), None)(f))
    assert(None === map2Match(None, Some(2))(f))
    assert(None === map2Match(None, None)(f))
  }

  test("sequence") {
    assert(Some(List(1, 2, 3)) === sequence(List(Some(1), Some(2), Some(3))))
    assert(None === sequence(List(None)))
    assert(Some(Nil) === sequence(Nil))
    assert(None === sequence(List(Some(1), Some(2), None)))
    assert(None === sequence(List(Some(1), None, Some(3))))
  }

  test("traverse") {
    val f = (x: Int) => if (x > 0) Some(x.toString) else None
    assert(Some(List("1", "2", "3")) === traverse(List(1, 2, 3))(f))
    assert(None === traverse(List(1, 0, 3))(f))
    assert(Some(Nil) === traverse(List())(f))
  }

  test("traverseMap") {
    val f = (x: Int) => if (x > 0) Some(x.toString) else None
    assert(Some(List("1", "2", "3")) === traverseMap(List(1, 2, 3))(f))
    assert(None === traverseMap(List(1, 0, 3))(f))
    assert(Some(Nil) === traverseMap(List())(f))
  }

  test("seqTraverse") {
    assert(Some(List(1, 2, 3)) === seqTraverse(List(Some(1), Some(2), Some(3))))
    assert(None === seqTraverse(List(None)))
    assert(Some(Nil) === seqTraverse(Nil))
    assert(None === seqTraverse(List(Some(1), Some(2), None)))
    assert(None === seqTraverse(List(Some(1), None, Some(3))))
  }
}
