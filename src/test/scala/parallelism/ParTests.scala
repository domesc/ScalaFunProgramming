package parallelism

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.{FlatSpec, Matchers}
import parallelism.Par.UnitFuture

/**
  * Created by domesc on 07/06/17.
  */
class ParTests extends FlatSpec with Matchers {

  behavior of "Par"

  val pool: ExecutorService = Executors.newFixedThreadPool(2)

  it should "combine two Par instances" in {
    val op1 = (es: ExecutorService) => UnitFuture(2000)
    val op2 = (es: ExecutorService) => UnitFuture(3000)

    val result = Par.map2(op1, op2)(_ + _)
    Par.run(result)(pool).get shouldEqual 5000
  }

  it should "filter in parallel from a List" in {
    val list = List(0, 1, 2, 3, 4, 5, 6)
    val result = Par.parFilter(list)(_ % 2 == 0)
    Par.run(result)(pool).get shouldEqual List(0, 2, 4, 6)
  }

}
