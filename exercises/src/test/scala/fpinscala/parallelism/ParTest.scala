package fpinscala.parallelism

import java.util.concurrent.Executors

import fpinscala.parallelism.Par
import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar

/**
 * Par unit tests.
 */
class ParTest extends FunSuite with MockitoSugar {

  test("map2") {
    val a = Par.unit(10)
    val b = Par.unit(5)
    val func = Par.map2(a, b)((x: Int, y: Int) => x + y)
    val result = Par.run(Executors.newFixedThreadPool(2))(func)
    assert(5 === result.get)
  }

  test("asyncF") {
    val executor = Executors.newSingleThreadExecutor()
    val a = 10
    val f = (_:Int).toString()
    val sync = Par.syncF(f)(a)
    val async = Par.asyncF(f)(a)

    assert(sync.leftSide === true)
    assert(async.leftSide === false)
  }

  test("sequence") {

  }

  test("parFilter") {

  }
}
