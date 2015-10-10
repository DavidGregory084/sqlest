package sqlest.bench

import sqlest.extractor._
import org.openjdk.jmh.annotations.{ Benchmark, Scope, State }

@State(Scope.Benchmark)
class ExtractorBench extends ExtractorSyntax[Seq[Any]] {

  def intExtractorAtIndex(index: Int) = new CellExtractor[Seq[Any], Int] {
    def read(seq: Seq[Any]) = Option(seq(index)).map(value => Integer.parseInt(value.toString))
  }

  val constantExtractor: ConstantExtractor[Seq[Any], Int] = extractConstant(10)
  val intExtractor: CellExtractor[Seq[Any], Int] = intExtractorAtIndex(0)

  val seqRows = List.fill(50)(Seq(1, "a"))

  /** Benchmark extractHeadOption of ConstantExtractor[Seq[Any], Int] */
  @Benchmark def constantExtractorHeadOption(): Option[Int] =
    constantExtractor.extractHeadOption(seqRows)

  /** Benchmark extractAll of ConstantExtractor[Seq[Any], Int] */
  @Benchmark def constantExtractorAll(): List[Int] =
    constantExtractor.extractAll(seqRows)

  /** Benchmark extractHeadOption of CellExtractor[Seq[Any], Int] */
  @Benchmark def cellExtractorHeadOption(): Option[Int] =
    intExtractor.extractHeadOption(seqRows)

  /** Benchmark extractAll of CellExtractor[Seq[Any], Int] */
  @Benchmark def cellExtractorAll(): List[Int] =
    intExtractor.extractAll(seqRows)

}
