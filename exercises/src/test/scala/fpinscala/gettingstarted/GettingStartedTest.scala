package fpinscala.gettingstarted

import org.scalatest.FunSuite

import fpinscala.gettingstarted.MyModule.fib
import fpinscala.gettingstarted.PolymorphicFunctions._

/**
 * GettingStarted unit test.
 */
class GettingStartedTest extends FunSuite {

  test("fib") {
    assert(0 === fib(0))
    assert(1 === fib(1))
    assert(1 === fib(2))
    assert(5 === fib(5))
    assert(8 === fib(6))
  }

  test("isSorted") {
    assert(true === isSorted(Array(4, 3, 2, 1), (x: Int, y: Int) => x > y))
    assert(false === isSorted(Array(1, 2, 3, 4), (x: Int, y: Int) => x > y))
  }

  test("partial1") {
    val f = (x: Int, y: Int) => x + y
    assert(f(3, 4) === partial1(3, f)(4))
  }

  test("curry") {
    val f = (x: Int, y: Int) => x + y
    assert(f(3, 4) === curry(f)(3)(4))
  }

  test("uncurry") {
    val f : Int => Int => Int = a => b => a + b
    assert(f(3)(4) === uncurry(f)(3, 4))
  }

  test("compose") {
    val f1 = (_: Int).toDouble
    val f2 = (_: Double).toString
    assert(f2(f1(10)) === compose(f2, f1)(10))
  }
}
