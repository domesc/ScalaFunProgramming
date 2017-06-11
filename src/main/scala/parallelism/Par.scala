package parallelism

import java.util.concurrent._

/**
  * Created by domesc on 07/06/17.
  */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](op: Par[A])(es: ExecutorService): Future[A] = op(es)

  /** Take an unevaluated computation and return a [[Par]] object which can run in a separate thread */
  def unit[A](a: => A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  case class UnitFuture[A](value: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(): A = value

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  /** Fork the computation in a separate thread */
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  /**
    * Combines the result of two parallel operations
    */
  def map2[A, B, C](op1: Par[A], op2: Par[B])(combine: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {

      val future1 = op1(es)
      val future2 = op2(es)

      UnitFuture(combine(future1.get, future2.get))
    }
  }

  /** Wraps its unevaluated argument in a [[Par]] and marks it for concurrent evaluation */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
    * Convert any function to one that evaluates its result asynchronously
    */
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  /** Collect asynchronous results */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldLeft(unit(List.empty[A])){case (acc, x) => map2(acc, x)(_ :+ _)}
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /** Filter elements of a [[List]] in parallel */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val parList = parMap(as)(a => if(f(a)) List(a) else List.empty[A])
    map2(parList, unit(()))((a, _) => a.flatten)
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
