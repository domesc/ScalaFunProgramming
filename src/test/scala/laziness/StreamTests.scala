package laziness

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dscarmozzino on 13/02/2017.
  */
class StreamTests extends FlatSpec with Matchers {

  behavior of "Stream"

  it should "convert a stream to a list" in {
    val input1 = Stream(1, 2, 3, 4, 5)
    input1.toList shouldEqual List(1, 2, 3, 4, 5)

    val input2 = Empty
    input2.toList shouldEqual List()
  }

  it should "take first n elements from the stream" in {
    val input1 = Stream(1, 2, 3, 4, 5)
    input1.take(3).toList shouldEqual List(1, 2, 3)

    val input2 = Empty
    input2.take(3).toList shouldEqual List()
  }

  it should "drop first n elements from the stream" in {
    val input1 = Stream(1, 2, 3, 4, 5)
    input1.drop(3).toList shouldEqual List(4, 5)

    val input2 = Empty
    input2.drop(3).toList shouldEqual List()
  }

  it should "take the first elements while the predicate is true" in {
    val input1 = Stream(1, 2, 3, 4, 5)
    input1.takeWhile(_ < 4).toList shouldEqual List(1, 2, 3)
    input1.takeWhileWithFold(_ < 4).toList shouldEqual List(1, 2, 3)

    val input2 = Stream.empty[Int]
    input2.takeWhile(_ < 4).toList shouldEqual List()
    input2.takeWhileWithFold(_ < 4).toList shouldEqual List()
  }

  it should "check all elements of the stream match the given predicate" in {
    val input1 = Stream(1, 3, 3, 5, 7)
    input1.forAll(_ % 2 != 0) shouldEqual true

    val input2 = Stream.empty[Int]
    input2.forAll(_ % 2 != 0) shouldEqual true

    val input3 = Stream(1, 3, 2, 5, 7)
    input3.forAll(_ % 2 != 0) shouldEqual false
  }

  it should "get the None if empty othewise an Option containing the value of the head" in {
    val input1 = Stream(1, 3, 3, 5, 7)
    input1.headOption shouldEqual Some(1)

    val input2 = Stream.empty[Int]
    input2.headOption shouldEqual None
  }

  it should "correctly map the elements of a stream" in {
    val input1 = Stream(1, 3, 3, 5, 7)
    input1.map(el => el + 2).toList shouldEqual List(3, 5, 5, 7, 9)

    val input2 = Stream.empty[Int]
    input2.map(el => el + 2).toList shouldEqual List()
  }

  it should "correctly filter the elements of a stream" in {
    val input1 = Stream(1, 2, 2, 7, 8)
    input1.filter(el => el % 2 == 0).toList shouldEqual List(2, 2, 8)

    val input2 = Stream.empty[Int]
    input2.filter(el => el % 2 == 0).toList shouldEqual List()
  }

  it should "correctly append an element to the end of a stream" in {
    val input = Stream(1, 2, 2, 7, 8)
    input.append(Stream(10, 14)).toList shouldEqual List(1, 2, 2, 7, 8, 10, 14)
  }

  it should "correctly flatMap a stream of streams" in {
    val input = Stream(Stream(1), Stream(2, 2), Stream(7), Stream(8))
    input.flatMap(identity).toList shouldEqual List(1, 2, 2, 7, 8)
  }

  it should "create an infinite stream from a given element" in {
    val infiniteStream = Stream.constant(1)
    infiniteStream.take(5).toList shouldEqual List(1, 1, 1, 1, 1)
  }

  it should "create an infinite stream from a given element and increment by 1" in {
    val infiniteStream = Stream.from(1)
    infiniteStream.take(5).toList shouldEqual List(1, 2, 3, 4, 5)
  }

  it should "create the infinite stream for fibonacci numbers" in {
    val fibonacci = Stream.fibs
    fibonacci.take(5).toList shouldEqual List(0, 1, 1, 2, 3)
  }

  it should "create a stream with unfold function" in {
    val output = Stream.unfold[Int, Int](10)(s => Option((s + 2, s + 2)))
    output.take(5).toList shouldEqual List(12, 14, 16, 18, 20)
  }

  it should "create the infinite stream for fibonacci numbers using unfold" in {
    val fibonacci = Stream.fibsWithUnfold
    fibonacci.take(5).toList shouldEqual List(0, 1, 1, 2, 3)
  }

  it should "create an infinite stream from a given element and increment by 1 using unfold" in {
    val infiniteStream = Stream.fromWithUnfold
    infiniteStream.take(5).toList shouldEqual List(1, 2, 3, 4, 5)
  }
}
