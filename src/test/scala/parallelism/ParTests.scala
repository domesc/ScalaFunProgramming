package parallelism

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.{AsyncFlatSpec, Matchers}

import scala.concurrent.Future

/**
  * Created by domesc on 07/06/17.
  */
class ParTests extends AsyncFlatSpec with Matchers {

  behavior of "Par"

  val pool: ExecutorService = Executors.newFixedThreadPool(2)

  it should "combine two Par instances" in {
    val op1 = (es: ExecutorService) => Future(2000)
    val op2 = (es: ExecutorService) => Future(3000)

    val result = Par.map2(op1, op2)(_ + _)
    Par.run(result)(pool) map (res => res shouldEqual 5000)
  }

}
