/*
 * Copyright 2014 JHC Systems Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sqlest.extractor

import org.scalatest._
import org.scalatest.matchers._

class ExtractorSpec extends FlatSpec with Matchers with ExtractorSyntax[Seq[Any]] {

  def intExtractorAtIndex(index: Int) = new CellExtractor[Seq[Any], Int] {
    def read(seq: Seq[Any]) = Option(seq(index)).map(value => Integer.parseInt(value.toString))
  }

  def stringExtractorAtIndex(index: Int) = new CellExtractor[Seq[Any], String] {
    def read(seq: Seq[Any]) = Option(seq(index)).map(_.toString)
  }

  "ConstantExtractor" should "extract the passed parameter for every row" in {
    val seqRows = List(Seq(), Seq(), Seq())
    val constantExtractor: ConstantExtractor[Seq[Any], Int] = extractConstant(10)
    constantExtractor.extractHeadOption(Nil) should be(None)
    constantExtractor.extractHeadOption(seqRows) should be(Some(10))
    constantExtractor.extractAll(seqRows) should be(List(10, 10, 10))
  }

  "CellExtractor" should "extract the value for the cell from each row" in {
    val seqRows = List(
      Seq(1, "a"),
      Seq(3, "c"),
      Seq(-1, "e")
    )

    val intExtractor: CellExtractor[Seq[Any], Int] = intExtractorAtIndex(0)
    intExtractor.extractHeadOption(Nil) should be(None)
    intExtractor.extractHeadOption(seqRows) should be(Some(1))
    intExtractor.extractAll(seqRows) should be(List(1, 3, -1))

    val stringExtractor: CellExtractor[Seq[Any], String] = stringExtractorAtIndex(1)
    stringExtractor.extractHeadOption(Nil) should be(None)
    stringExtractor.extractHeadOption(seqRows) should be(Some("a"))
    stringExtractor.extractAll(seqRows) should be(List("a", "c", "e"))
  }

  it should "throw a NullPointerException if it reads a null value" in {
    val seqRows = List(Seq(null, null), Seq(2, "a"), Seq(4, "b"))
    val intExtractor: CellExtractor[Seq[Any], Int] = intExtractorAtIndex(0)
    val stringExtractor: CellExtractor[Seq[Any], String] = stringExtractorAtIndex(1)

    intercept[NullPointerException] {
      intExtractor.extractHeadOption(seqRows)
    }

    intercept[NullPointerException] {
      stringExtractor.extractHeadOption(seqRows)
    }

    intercept[NullPointerException] {
      intExtractor.extractAll(seqRows)
    }

    intercept[NullPointerException] {
      stringExtractor.extractAll(seqRows)
    }
  }

  "MappedExtractor" should "apply a function to a value when extracting it" in {
    val seqRows = List(Seq(0, "hello"), Seq(2, "bye"), Seq(4, "level"))
    val mappedIntExtractor: MappedExtractor[Seq[Any], Int, Boolean] = intExtractorAtIndex(0).map(_ == 2)
    val mappedStringExtractor: MappedExtractor[Seq[Any], String, String] = stringExtractorAtIndex(1).map(_.reverse)

    mappedIntExtractor.extractHeadOption(Nil) should be(None)
    mappedIntExtractor.extractHeadOption(seqRows) should be(Some(false))
    mappedIntExtractor.extractAll(seqRows) should be(List(false, true, false))

    mappedStringExtractor.extractHeadOption(Nil) should be(None)
    mappedStringExtractor.extractHeadOption(seqRows) should be(Some("olleh"))
    mappedStringExtractor.extractAll(seqRows) should be(List("olleh", "eyb", "level"))
  }

  it should "throw a NullPointerException if the inner extractor extracted a null value" in {
    val seqRows = List(Seq(null), Seq(2), Seq(null))
    val mappedIntExtractor: MappedExtractor[Seq[Any], Int, Boolean] = intExtractorAtIndex(0).map(_ == 2)

    intercept[NullPointerException] {
      mappedIntExtractor.extractHeadOption(seqRows)
    }

    intercept[NullPointerException] {
      mappedIntExtractor.extractAll(seqRows)
    }
  }

  "AppedExtractor" should "extract the result of applying a function extractor over a value extractor" in {
    val seqRows = List(Seq(0, "hello"), Seq(2, "bye"), Seq(4, "level"))

    val appedIntExtractor = intExtractorAtIndex(0).ap(extractConstant[Int => Boolean](_ == 2))
    val appedStringExtractor = stringExtractorAtIndex(1).ap(extractConstant[String => String](_.reverse))

    appedIntExtractor.extractHeadOption(Nil) should be(None)
    appedIntExtractor.extractHeadOption(seqRows) should be(Some(false))
    appedIntExtractor.extractAll(seqRows) should be(List(false, true, false))

    appedStringExtractor.extractHeadOption(Nil) should be(None)
    appedStringExtractor.extractHeadOption(seqRows) should be(Some("olleh"))
    appedStringExtractor.extractAll(seqRows) should be(List("olleh", "eyb", "level"))
  }

  it should "extract the result of applying a curried function over several extractors" in {
    val seqRows1 = List(Seq(0, "hello"), Seq(2, "bye"), Seq(4, "level"))
    val seqRows2 = List(Seq(5, "hello"), Seq(3, "bye"), Seq(5, "level"))

    val col1 = intExtractorAtIndex(0)
    val col2 = stringExtractorAtIndex(1)
    val fn = extractConstant[Int => String => Boolean](i => s => i == s.length)

    val appedExtractor = col2.ap(col1.ap(fn))

    appedExtractor.extractHeadOption(Nil) should be(None)
    appedExtractor.extractHeadOption(seqRows1) should be(Some(false))
    appedExtractor.extractHeadOption(seqRows2) should be(Some(true))
    appedExtractor.extractAll(seqRows1) should be(List(false, false, false))
    appedExtractor.extractAll(seqRows2) should be(List(true, true, true))
  }

  "ChoiceExtractor" should "extract using left when the predicate is true and right when it is false" in {
    val seqRows = List(Seq(0, "a"), Seq(1, "b"), Seq(2, "c"), Seq(4, "e"), Seq(5, "f"), Seq(7, "h"))
    val addIntExtractor = intExtractorAtIndex(0).map(_ + 2)
    val multIntExtractor = intExtractorAtIndex(0).map(_ * 2)
    val choiceExtractor = intExtractorAtIndex(0).choose(_ % 2 == 0)(addIntExtractor, multIntExtractor)

    choiceExtractor.extractHeadOption(Nil) should be(None)
    choiceExtractor.extractHeadOption(seqRows) should be(Some(2))
    choiceExtractor.extractAll(seqRows) should be(List(2, 2, 4, 6, 10, 14))
  }

  it should "extract from a list of choices when .switch is used" in {
    val seqRows = List(Seq(0, "a"), Seq(1, "b"), Seq(2, "c"), Seq(4, "e"), Seq(5, "f"), Seq(7, "h"))
    val addIntExtractor = intExtractorAtIndex(0).map(_ + 2)
    val multIntExtractor = intExtractorAtIndex(0).map(_ * 2)
    val choiceExtractor = intExtractorAtIndex(0).choose(_ % 2 == 0)(addIntExtractor, multIntExtractor)

    intExtractorAtIndex(0).switch(
      0 -> addIntExtractor,
      1 -> addIntExtractor,
      2 -> multIntExtractor,
      4 -> addIntExtractor,
      5 -> addIntExtractor,
      7 -> multIntExtractor
    ).extractAll(seqRows) should be(List(2, 3, 4, 6, 7, 14))
  }

  it should "throw an exception when extracting a case that is not specified with .switch" in {
    val seqRows = List(Seq(0, "a"), Seq(1, "b"), Seq(2, "c"), Seq(4, "e"), Seq(5, "f"), Seq(7, "h"))
    val addIntExtractor = intExtractorAtIndex(0).map(_ + 2)
    val multIntExtractor = intExtractorAtIndex(0).map(_ * 2)
    val choiceExtractor = intExtractorAtIndex(0).choose(_ % 2 == 0)(addIntExtractor, multIntExtractor)

    intercept[MatchError] {
      intExtractorAtIndex(0).switch(
        0 -> addIntExtractor,
        1 -> multIntExtractor
      ).extractAll(seqRows)
    }
  }

  it should "extract from a list of choices using predicates when .cond is used" in {
    val seqRows = List(Seq(0, "a"), Seq(1, "b"), Seq(3, "c"), Seq(4, "e"), Seq(5, "f"), Seq(7, "h"))
    val addIntExtractor = intExtractorAtIndex(0).map(_ + 2)
    val multIntExtractor = intExtractorAtIndex(0).map(_ * 2)
    val squareIntExtractor = intExtractorAtIndex(0).map(Math.pow(_, 2))
    val choiceExtractor = intExtractorAtIndex(0).choose(_ % 2 == 0)(addIntExtractor, multIntExtractor)

    val greaterThanFour = (i: Int) => i > 4
    val divisibleByThree = (i: Int) => i % 3 == 0
    val fallBack = (i: Int) => true

    intExtractorAtIndex(0).cond(
      greaterThanFour -> addIntExtractor,
      divisibleByThree -> squareIntExtractor,
      fallBack -> multIntExtractor
    ).extractAll(seqRows) should be(List(0, 2, 9, 8, 7, 9))
  }

  it should "throw an exception when extracting a case that is not specified with .cond" in {
    val seqRows = List(Seq(0, "a"), Seq(1, "b"), Seq(3, "c"), Seq(4, "e"), Seq(5, "f"), Seq(7, "h"))
    val addIntExtractor = intExtractorAtIndex(0).map(_ + 2)
    val multIntExtractor = intExtractorAtIndex(0).map(_ * 2)
    val squareIntExtractor = intExtractorAtIndex(0).map(Math.pow(_, 2))
    val choiceExtractor = intExtractorAtIndex(0).choose(_ % 2 == 0)(addIntExtractor, multIntExtractor)

    val greaterThanFour = (i: Int) => i > 4
    val divisibleByThree = (i: Int) => i % 3 == 0

    intercept[MatchError] {
      intExtractorAtIndex(0).cond(
        greaterThanFour -> addIntExtractor,
        divisibleByThree -> squareIntExtractor
      ).extractAll(seqRows)
    }
  }

  "TupleExtractors" should "extract values from all extractors and return them in a tuple" in {
    val seqRows = List(Seq(0, "hello"), Seq(2, "bye"), Seq(4, "level"))
    val tuple3Extractor: Tuple3Extractor[Seq[Any], Int, String, Boolean] =
      extractTuple(
        intExtractorAtIndex(0),
        stringExtractorAtIndex(1).map(_.reverse),
        intExtractorAtIndex(0).map(_ == 2))

    tuple3Extractor.extractHeadOption(Nil) should be(None)
    tuple3Extractor.extractHeadOption(seqRows) should be(Some((0, "olleh", false)))
    tuple3Extractor.extractAll(seqRows) should be(List(
      (0, "olleh", false),
      (2, "eyb", true),
      (4, "level", false)
    ))
  }

  it should "create a MappedExtractor using the map method that works with the parameters not in a tuple" in {
    val seqRows = List(Seq(0, "hello"), Seq(2, "bye"), Seq(4, "level"))

    case class Triple(a: Int, b: String, c: Boolean)
    val tuple3Extractor: MappedExtractor[Seq[Any], (Int, String, Boolean), Triple] =
      extractTuple(
        intExtractorAtIndex(0),
        stringExtractorAtIndex(1).map(_.reverse),
        intExtractorAtIndex(0).map(_ == 2)
      ).map((a, b, c) => Triple(a, b, c))

    tuple3Extractor.extractHeadOption(Nil) should be(None)
    tuple3Extractor.extractHeadOption(seqRows) should be(Some(Triple(0, "olleh", false)))
    tuple3Extractor.extractAll(seqRows) should be(List(
      Triple(0, "olleh", false),
      Triple(2, "eyb", true),
      Triple(4, "level", false)
    ))
  }

  it should "receive an unapply method as an optional argument to map" in {
    stringExtractorAtIndex(1).map(_.reverse, (s: String) => Some(s.reverse))
  }

  it should "throw a NullPointerException if any of the inner extractors extracted a null value" in {
    val seqRows = List(Seq(0, null), Seq(2, "bye"), Seq(4, "level"))

    case class Triple(a: Int, b: String, c: Boolean)
    val tuple3Extractor: MappedExtractor[Seq[Any], (Int, String, Boolean), Triple] =
      extractTuple(
        intExtractorAtIndex(0),
        stringExtractorAtIndex(1).map(_.reverse),
        intExtractorAtIndex(0).map(_ == 2)
      ).map(Triple)

    intercept[NullPointerException] {
      tuple3Extractor.extractHeadOption(seqRows)
    }

    intercept[NullPointerException] {
      tuple3Extractor.extractAll(seqRows)
    }
  }

  "SeqExtractor" should "extract values from all extractors and return them in a Seq" in {
    val seqRows = List(Seq(0, 1, 2), Seq(2, 3, 4), Seq(5, 6, 7))
    val seqExtractor: SeqExtractor[Seq[Any], Int] =
      SeqExtractor(Seq(
        intExtractorAtIndex(0),
        intExtractorAtIndex(1),
        intExtractorAtIndex(2).map(_ * 2)))

    seqExtractor.extractHeadOption(Nil) should be(None)
    seqExtractor.extractHeadOption(seqRows) should be(Some(Seq(0, 1, 4)))
    seqExtractor.extractAll(seqRows) should be(List(
      Seq(0, 1, 4),
      Seq(2, 3, 8),
      Seq(5, 6, 14)
    ))
  }

  it should "throw a NullPointerException if any of the inner extractors extracted a null value" in {
    val seqRows = List(Seq(null, 1, 2), Seq(2, 3, 4), Seq(5, 6, null))
    val seqExtractor: SeqExtractor[Seq[Any], Int] =
      SeqExtractor(Seq(
        intExtractorAtIndex(0),
        intExtractorAtIndex(1),
        intExtractorAtIndex(2).map(_ * 2)))

    intercept[NullPointerException] {
      seqExtractor.extractHeadOption(seqRows)
    }

    intercept[NullPointerException] {
      seqExtractor.extractAll(seqRows)
    }
  }

  "OptionExtractor" should "wrap a ConstantExtractor and return Some of the constant value" in {
    val seqRows = List(Seq(), Seq(), Seq())
    val optionConstantExtractor: OptionExtractor[Seq[Any], Int] = extractConstant(10).asOption

    optionConstantExtractor.extractHeadOption(Nil) should be(None)
    optionConstantExtractor.extractHeadOption(seqRows) should be(Some(Some(10)))
    optionConstantExtractor.extractAll(seqRows) should be(List(Some(10), Some(10), Some(10)))
  }

  it should "wrap a CellExtractor and wrap any null value in an Option" in {
    val seqRows = List(Seq(null), Seq(2), Seq(null))
    val optionIntExtractor: OptionExtractor[Seq[Any], Int] = intExtractorAtIndex(0).asOption

    optionIntExtractor.extractHeadOption(Nil) should be(None)
    optionIntExtractor.extractHeadOption(seqRows) should be(Some(None))
    optionIntExtractor.extractAll(seqRows) should be(List(None, Some(2), None))
  }

  it should "wrap a MappedExtractor and wrap any null value in an Option" in {
    val seqRows = List(Seq(null), Seq(2), Seq(4))
    val optionMappedIntExtractor: OptionExtractor[Seq[Any], Boolean] = intExtractorAtIndex(0).map(_ == 2).asOption

    optionMappedIntExtractor.extractHeadOption(Nil) should be(None)
    optionMappedIntExtractor.extractHeadOption(seqRows) should be(Some(None))
    optionMappedIntExtractor.extractAll(seqRows) should be(List(None, Some(true), Some(false)))
  }

  it should "wrap a TupleExtractor and wrap any null value in an Option" in {
    val seqRows = List(Seq(0, null), Seq(2, "bye"), Seq(4, "level"))

    case class Triple(a: Int, b: String, c: Boolean)
    val tuple3Extractor: OptionExtractor[Seq[Any], (Int, String, Boolean)] =
      extractTuple(
        intExtractorAtIndex(0),
        stringExtractorAtIndex(1).map(_.reverse),
        intExtractorAtIndex(0).map(_ == 2)
      ).asOption

    tuple3Extractor.extractHeadOption(Nil) should be(None)
    tuple3Extractor.extractHeadOption(seqRows) should be(Some(None))
    tuple3Extractor.extractAll(seqRows) should be(List(
      None,
      Some((2, "eyb", true)),
      Some((4, "level", false))
    ))
  }

  it should "wrap a SeqExtractor and wrap any null value in an Option" in {
    val seqRows = List(Seq(null, 1, 2), Seq(2, 3, 4), Seq(5, 6, null))
    val seqExtractor: OptionExtractor[Seq[Any], Seq[Int]] =
      SeqExtractor(Seq(
        intExtractorAtIndex(0),
        intExtractorAtIndex(1),
        intExtractorAtIndex(2).map(_ * 2)
      )).asOption

    seqExtractor.extractHeadOption(Nil) should be(None)
    seqExtractor.extractHeadOption(seqRows) should be(Some(None))
    seqExtractor.extractAll(seqRows) should be(List(
      None,
      Some(List(2, 3, 8)),
      None
    ))
  }

  "ListMultiRowExtractor" should "wrap the value from each row in a list when not in a GroupedExtractor" in {
    val seqRows = List(
      Seq(1, "a"),
      Seq(3, "c"),
      Seq(-1, "e")
    )

    val intListExtractor: ListMultiRowExtractor[Seq[Any], Int] = intExtractorAtIndex(0).asList
    intListExtractor.extractHeadOption(Nil) should be(None)
    intListExtractor.extractHeadOption(seqRows) should be(Some(List(1)))
    intListExtractor.extractAll(seqRows) should be(List(List(1), List(3), List(-1)))

    val stringListExtractor: ListMultiRowExtractor[Seq[Any], String] = stringExtractorAtIndex(1).asList
    stringListExtractor.extractHeadOption(Nil) should be(None)
    stringListExtractor.extractHeadOption(seqRows) should be(Some(List("a")))
    stringListExtractor.extractAll(seqRows) should be(List(List("a"), List("c"), List("e")))
  }

  it should "be composable with an OptionExtractor" in {
    val seqRows = List(Seq(null), Seq(3), Seq(null), Seq(-1))

    val intListExtractor: ListMultiRowExtractor[Seq[Any], Option[Int]] = intExtractorAtIndex(0).asOption.asList
    intListExtractor.extractHeadOption(Nil) should be(None)
    intListExtractor.extractHeadOption(seqRows) should be(Some(List(None)))
    intListExtractor.extractAll(seqRows) should be(List(
      List(None),
      List(Some(3)),
      List(None),
      List(Some(-1))
    ))
  }

  it should "work with another ListMultiRowExtractor as peers within another extractor" in {
    val seqRows = List(Seq(0, "hello"), Seq(2, "bye"), Seq(4, "level"))
    val tupleOfListExtractors: Tuple2Extractor[Seq[Any], List[Int], List[String]] =
      extractTuple(intExtractorAtIndex(0).asList, stringExtractorAtIndex(1).asList)

    tupleOfListExtractors.extractHeadOption(Nil) should be(None)
    tupleOfListExtractors.extractHeadOption(seqRows) should be(Some(List(0), List("hello")))
    tupleOfListExtractors.extractAll(seqRows) should be(List(
      (List(0), List("hello")),
      (List(2), List("bye")),
      (List(4), List("level"))
    ))
  }

  "ListMultiRowExtractor within a GroupedExtractor" should "accumulate all values with the same group by value into a list" in {
    val seqRows = List(Seq(0, "first"), Seq(0, "second"), Seq(1, "third"), Seq(2, "forth"), Seq(2, "fifth"))
    val groupedExtractor: GroupedExtractor[Seq[Any], (Int, List[String]), Int] =
      extractTuple(
        intExtractorAtIndex(0),
        stringExtractorAtIndex(1).asList
      ).groupBy(intExtractorAtIndex(0))

    groupedExtractor.extractHeadOption(Nil) should be(None)
    groupedExtractor.extractHeadOption(seqRows) should be(Some((0, List("first", "second"))))
    groupedExtractor.extractAll(seqRows) should be(List(
      (0, List("first", "second")),
      (1, List("third")),
      (2, List("forth", "fifth"))
    ))
  }

  it should "nest with another ListMultiRowExtractor" in {
    val seqRows = List(
      Seq(1, 1, 1),
      Seq(1, 1, 2),
      Seq(1, 2, 3),
      Seq(1, 2, 4),
      Seq(2, 3, 5),
      Seq(2, 3, 6),
      Seq(2, 4, 7),
      Seq(2, 4, 8)
    )

    val nestedListExtractor = extractTuple(
      intExtractorAtIndex(0),
      extractTuple(
        intExtractorAtIndex(1),
        intExtractorAtIndex(2).asList
      ).asList
    ).groupBy(intExtractorAtIndex(0))

    nestedListExtractor.extractHeadOption(Nil) should be(None)
    nestedListExtractor.extractHeadOption(seqRows) should be(Some((1, List((1, List(1)), (1, List(2)), (2, List(3)), (2, List(4))))))
    nestedListExtractor.extractAll(seqRows) should be(List(
      (1,
        List(
          (1, List(1)),
          (1, List(2)),
          (2, List(3)),
          (2, List(4)))),
      (2,
        List(
          (3, List(5)),
          (3, List(6)),
          (4, List(7)),
          (4, List(8))))
    ))
  }

  it should "stop in extractHeadOption when group by value changes" in {
    val seqRows = List(Seq(0, "first"), Seq(0, "second"), Seq(1, "third"), Seq(0, "forth"))
    val groupedExtractor: GroupedExtractor[Seq[Any], (Int, List[String]), Int] =
      extractTuple(
        intExtractorAtIndex(0),
        stringExtractorAtIndex(1).asList
      ).groupBy(intExtractorAtIndex(0))

    groupedExtractor.extractHeadOption(Nil) should be(None)
    groupedExtractor.extractHeadOption(seqRows) should be(Some((0, List("first", "second"))))
  }

  it should "return an empty list if all inner values are null values" in {
    val seqRows = List(Seq(0, null), Seq(0, null), Seq(1, "third"), Seq(2, null))
    val groupedExtractor: GroupedExtractor[Seq[Any], (Int, List[String]), Int] =
      extractTuple(
        intExtractorAtIndex(0),
        stringExtractorAtIndex(1).asList
      ).groupBy(intExtractorAtIndex(0))

    groupedExtractor.extractHeadOption(Nil) should be(None)
    groupedExtractor.extractHeadOption(seqRows) should be(Some((0, Nil)))
    groupedExtractor.extractAll(seqRows) should be(List(
      (0, Nil),
      (1, List("third")),
      (2, Nil)
    ))
  }

  it should "throw a NullPointerException if some but not all of the inner values are null values" in {
    val seqRows = List(Seq(0, "first"), Seq(0, null), Seq(1, "third"), Seq(2, null))
    val groupedExtractor: GroupedExtractor[Seq[Any], (Int, List[String]), Int] =
      extractTuple(
        intExtractorAtIndex(0),
        stringExtractorAtIndex(1).asList
      ).groupBy(intExtractorAtIndex(0))

    intercept[NullPointerException] {
      groupedExtractor.extractHeadOption(seqRows) should be(Some((0, Nil)))
    }

    intercept[NullPointerException] {
      groupedExtractor.extractAll(seqRows)
    }
  }

  it should "return an empty list if the group by value is null" in {
    val seqNullRows = List(Seq(null, null, null, null), Seq(null, null, null, null))
    val seqRows = List(Seq(0, "first", null, null), Seq(0, "second", null, null), Seq(1, "third", 10, "innerThird"), Seq(1, "fourth", 10, "innerFourth"))
    val groupedExtractor: GroupedExtractor[Seq[Any], (Int, List[String], List[(Int, List[String])]), Int] =
      extractTuple(
        intExtractorAtIndex(0),
        stringExtractorAtIndex(1).asList,
        extractTuple(
          intExtractorAtIndex(2),
          stringExtractorAtIndex(3).asList
        ).groupBy(intExtractorAtIndex(2))
      ).groupBy(intExtractorAtIndex(0))

    groupedExtractor.extractHeadOption(Nil) should be(None)
    groupedExtractor.extractHeadOption(seqNullRows) should be(None)
    groupedExtractor.extractHeadOption(seqRows) should be(Some(0, List("first", "second"), Nil))
    groupedExtractor.extractAll(seqRows) should be(List(
      (0, List("first", "second"), Nil),
      (1, List("third", "fourth"), List((10, List("innerThird", "innerFourth"))))
    ))
  }
}
