package fpinscala.errorhandling

import org.scalatest.FunSuite

import fpinscala.errorhandling.Either._

/**
 * Either unit tests.
 */
class EitherTest extends FunSuite {
  test("map") {
    val f = (x: Int) => x * 2
    assert(Right(1).map(f) === Right(2))
    assert(Left(1).map(f) === Left(1))
  }

  test("flatMap") {
    val f = (x: Int) => Right(x * 2)
    assert(Right(1).flatMap(f) === Right(2))
    assert(Left(1).flatMap(f) === Left(1))
  }

  test("orElse") {
    assert(Right(1).orElse(Right(2)) === Right(1))
    assert(Left(1).orElse(Right(2)) === Right(2))
  }

  test("map2") {
    val f = (x: Int, y: Int) => x + y
    assert(Right(3) === Right(1).map2(Right(2))(f))
    assert(Left(2) === Right(1).map2(Left(2))(f))
    assert(Left(1) === Left(1).map2(Right(2))(f))
    assert(Left(1) === Left(1).map2(Left(2))(f))
  }

  test("sequence") {
    assert(Right(List(1, 2, 3)) === sequence(List(Right(1), Right(2), Right(3))))
    assert(Left(1) === sequence(List(Left(1))))
    assert(Right(Nil) === sequence(Nil))
    assert(Left(1) === sequence(List(Right(1), Right(2), Left(1))))
    assert(Left(2) === sequence(List(Right(1), Left(2), Right(3))))
  }

  test("traverse") {
    val f = (x: Int) => if (x > 0) Right(x.toString) else Left(x)
    assert(Right(List("1", "2", "3")) === traverse(List(1, 2, 3))(f))
    assert(Left(0) === traverse(List(1, 0, 3))(f))
    assert(Right(Nil) === traverse(Nil)(f))
  }
}
