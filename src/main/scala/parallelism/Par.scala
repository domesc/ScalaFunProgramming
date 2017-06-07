package parallelism

import java.util.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by domesc on 07/06/17.
  */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](op: Par[A])(es: ExecutorService): Future[A] = op(es)

  /** Take an unevaluated computation and return a [[Par]] object which can run in a separate thread */
  def unit[A](a: => A): Par[A] = (es: ExecutorService) => Future(a)

  def map2[A, B, C](op1: Par[A], op2: Par[B])(combine: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {

      for {
        future1 <- op1(es)
        future2 <- op2(es)
      } yield combine(future1, future2)
    }
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if(ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
  }

}
