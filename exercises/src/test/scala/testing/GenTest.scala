package testing

import fpinscala.state.RNG._
import fpinscala.state.State
import fpinscala.testing.Prop.{Passed, Falsified}
import fpinscala.testing.{Prop, SimpleProp, Gen}
import org.scalatest.FunSuite

/**
 * Gen unit tests.
 */
class GenTest extends FunSuite {

  test("SimpleProp") {
    assert(true === (SimplePropT() && SimplePropT()).check)
    assert(false === (SimplePropF() && SimplePropT()).check)
    assert(false === (SimplePropT() && SimplePropF()).check)
    assert(false === (SimplePropF() && SimplePropF()).check)
  }

  case class SimplePropT() extends SimpleProp {
    override def check: Boolean = true
  }

  case class SimplePropF() extends SimpleProp {
    override def check: Boolean = false
  }

  test("Gen.unit") {
    val unit = Gen.unit(1)
    assert(1 === runGen(unit))
  }

  test("Gen.listOfN") {
    val ra: Rand[Int] = (1, _)
    val gen = Gen(State(ra))
    assert(List(1, 1, 1, 1, 1) === runGen(Gen.listOfN(5, gen)))
  }

  test("Gen.boolean") {
    assert(runGen(Gen.boolean).isInstanceOf[Boolean])
  }

  def runGen[A](gen: Gen[A]): A = {
    val rng = Simple(10)
    gen.sample.run(rng)._1
  }

  test("Logical operator Prop") {
    val valid = Prop((tc, rng) => Passed)
    val invalid = Prop((tc, rng) => Falsified("", 0))

    assert(Passed === (valid && valid).run(0, Simple(1)))
    assert(Falsified(" (LeftProp)", 0) === (invalid && valid).run(0, Simple(1)))
    assert(Falsified(" (RightProp)", 0) === (valid && invalid).run(0, Simple(1)))
    assert(Falsified(" (LeftProp)", 0) === (invalid && valid).run(0, Simple(1)))

    assert(Passed === (valid || valid).run(0, Simple(1)))
    assert(Passed === (invalid || valid).run(0, Simple(1)))
    assert(Passed === (valid || invalid).run(0, Simple(1)))
    assert(Falsified("", 0) === (invalid || invalid).run(0, Simple(1)))
  }

}
