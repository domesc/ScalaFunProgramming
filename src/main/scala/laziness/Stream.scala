package laziness

sealed trait Stream[+A] {
  /** Write a function to convert a Stream to a List */
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() +: t().toList
  }

  /** Write the function for returning the first n elements of a Stream */
  def take(n: Int): Stream[A] = (n, this) match {
    case (0, _) => Empty
    case (_, Empty) => Empty
    case(_, Cons(h, t)) => Cons(h, () => t().take(n - 1))
  }

  /** Write the function drop(n) for skipping the first n elements of a [[Stream]] */
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = (n, this) match {
    case (_, Cons(_, t)) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /** Write the function takeWhile for returning all starting elements of a [[Stream]] that match the given predicate */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  /** Implement forAll, which checks that all elements in the Stream match a given predicate. The implementation should
    * terminate the traversal as soon as it encounters a non matching value.
    */
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => t().forAll(p)
    case Empty => true
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /** Use foldRight to implement takeWhile. */
  def takeWhileWithFold(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])(
    (h, t) => if(p(h)) Stream.cons(h, t) else t
  )

  /** Implement headOption using foldRight */
  def headOption: Option[A] = foldRight(Option.empty[A])(
    (h, _) => Option(h)
  )

  /** Implement map, filter, append and flatMap using foldRight. The append method should be non-strict in its arguments*/
  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])(
    (h, t) => Stream.cons(f(h), t)
  )

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])(
    (h, t) => if(f(h)) Stream.cons(h, t) else t
  )

  def append[B >: A](element: => Stream[B]): Stream[B] = foldRight(element)(
    (h, t) => Stream.cons(h, t)
  )
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])(
    (h, t) => f(h).append(t)
  )
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A])extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /** Returns an infinite stream */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /** Generates an infinite stream of integers incrementing by 1 */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /** Generates the infinite stream of fibonacci */
  val fibs: Stream[Int] = {
    def fibInternal(n0: Int, n1: Int): Stream[Int] = cons(n0, fibInternal(n1, n0 + n1))
    fibInternal(0, 1)
  }

  /**
    * This is a stream-building function called unfold. It takes an initial state,
    * and a function for producing both the next state and the next value in the generated
    * stream.
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => empty
  }

  val fibsWithUnfold: Stream[Int] = unfold[Int, (Int, Int)]((0, 1)){ case (fib1, fib2) => Option(fib1, (fib2, fib1 + fib2))}

  val fromWithUnfold: Stream[Int] = unfold(1)(init => Option(init, init + 1))
}
