package state

import fpinscala.state._
import org.scalatest.FunSuite

import fpinscala.state.RNG.Simple
import fpinscala.state.RNG._

/**
 * State unit tests.
 */
class StateTest extends FunSuite {
  def assertIntValid(n: Int) = assert(0 <= n && n <= Int.MaxValue)
  def assertDoubleValid(d: Double) = assert(0.0 <= d && d < 1.0)

  test("nonNegativeInt") {
    val rng = Simple(10L)
    assertIntValid(nonNegativeInt(rng)._1)
  }

  test("double") {
    val rng = Simple(10L)
    assertDoubleValid(double(rng)._1)
  }

  test("intDouble") {
    val rng = Simple(10L)
    val id = intDouble(rng)._1
    assertIntValid(id._1)
    assertDoubleValid(id._2)
  }

  test("doubleInt") {
    val rng = Simple(10L)
    val di = doubleInt(rng)._1
    assertDoubleValid(di._1)
    assertIntValid(di._2)
  }

  test("double3") {
    val rng = Simple(10L)
    val d3 = double3(rng)._1
    assertDoubleValid(d3._1)
    assertDoubleValid(d3._2)
    assertDoubleValid(d3._3)
  }

  test("ints") {
    val rng = Simple(10L)
    val a: List[Int] = ints(10)(rng)._1
    assert(10 === a.length)
    a.foreach(x => assert(Int.MinValue <= x && x <= Int.MaxValue))
  }

  test("doubleMap") {
    val rng = Simple(10L)
    assertDoubleValid(doubleMap(rng)._1)
  }

  test("map2") {
    val ra: Rand[Int] = (1, _)
    val rb: Rand[Int] = (2, _)
    val f = (x: Int, y: Int) => x + y
    val rng = Simple(10L)
    assert((3, rng) === map2(ra, rb)(f)(rng))
  }

  test("sequence") {
    val rng = Simple(10L)
    assert(List(1, 2, 3) === sequence(List(unit(1), unit(2), unit(3)))(rng)._1)
  }

  test("intsSeq") {
    val rng = Simple(10L)
    val a: List[Int] = intsSeq(10)(rng)(rng)._1 // we need to force evaluation
    assert(10 === a.length)
    a.foreach(x => assert(Int.MinValue <= x && x <= Int.MaxValue))
  }

  test("flatMap") {
    val rng = Simple(10L)

    def recursive(n: Int): Rand[Int] =
      rng => (n+1, rng)

    val result = flatMap(recursive(1))(x => {
      rng => {
        if (x == 3)
          (x, rng)
        else
          recursive(x)(rng)
      }
    })(rng)

    assert(result === (3, rng))
  }

  test("nonNegativeLessThan") {
    val rng = Simple(10L)
    val result = nonNegativeLessThan(5)(rng)._1
    assert(result < 5 && result >= 0)
  }

  test("flatMapMap2") {
    val ra: Rand[Int] = (1, _)
    val rb: Rand[Int] = (2, _)
    val f = (x: Int, y: Int) => x + y
    val rng = Simple(10L)
    assert((3, rng) === flatMapMap2(ra, rb)(f)(rng))
  }

  test("stateMap") {
    val ra: Rand[Int] = (1, _)
    val ras = State(ra)

    val f = (x: Int) => x + 1
    val rng = Simple(10L)
    assert((2, rng) === ras.map(f).run(rng))
  }

  test("stateMap2") {
    val ra: Rand[Int] = (1, _)
    val ras = State(ra)
    val rb: Rand[Int] = (2, _)
    val rbs = State(rb)

    val f = (x: Int, y: Int) => x + y
    val rng = Simple(10L)
    assert((3, rng) === ras.map2(rbs)(f).run(rng))
  }

  test("stateFlatMap") {
    val ra: Rand[Int] = (1, _)
    val ras = State(ra)

    val f = (x: Int) => State(RNG.unit(x + 1))
    val rng = Simple(10L)
    assert((2, rng) === ras.flatMap(f).run(rng))
  }
}
