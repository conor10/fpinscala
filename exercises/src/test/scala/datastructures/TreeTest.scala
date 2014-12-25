package datastructures

import org.scalatest.FunSuite

import fpinscala.datastructures.Branch
import fpinscala.datastructures.Leaf
import fpinscala.datastructures.Tree._

/**
 * Tree unit tests.
 */
class TreeTest extends FunSuite {
  test("size") {
    assert(7 === size(
      Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))
  }

  test("max") {
    assert(4 === max(
      Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(4), Leaf(2)))))
  }

  test("depth") {
    assert(3 === depth(
      Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))
  }

  test("map") {
    assert(Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8))) ===
      map(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))
        ((x: Int) => x * 2))
  }

  test("sizeFold") {
    assert(7 === sizeFold(
      Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))
  }

  test("maxFold") {
    assert(4 === maxFold(
      Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(4), Leaf(2)))))
  }

  test("depthFold") {
    assert(3 === depthFold(
      Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))
  }

  test("mapFold") {
    assert(Branch(Branch(Leaf(2), Leaf(4)), Branch(Leaf(6), Leaf(8))) ===
      mapFold(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))
        ((x: Int) => x * 2))
  }
}
