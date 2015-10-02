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

package sqlest.executor

import java.sql.ResultSet
import sqlest.ast._
import sqlest.ast.syntax._
import sqlest.extractor._

trait ExecutorSyntax extends QuerySyntax {
  implicit class SelectExecutorOps[A](select: Select[A, _ <: Relation])(implicit session: Session) {
    def fetchHead[SingleResult](implicit extractable: Extractable.Aux[ResultSet, A, SingleResult]): SingleResult =
      fetchHeadOption.getOrElse(throw new NoSuchElementException("fetchHead when no results returned"))

    def fetchHeadOption[SingleResult](implicit extractable: Extractable.Aux[ResultSet, A, SingleResult]): Option[SingleResult] =
      session.executeSelect(select)(row => extractable.extractor(select.cols).extractHeadOption(row))

    def fetchAll[SingleResult](implicit extractable: Extractable.Aux[ResultSet, A, SingleResult]): List[SingleResult] =
      session.executeSelect(select)(row => extractable.extractor(select.cols).extractAll(row))

    def extractHead(extractor: Extractor[ResultSet, _]): extractor.SingleResult =
      extractHeadOption(extractor).getOrElse(throw new NoSuchElementException("extractHead when no results returned"))

    def extractHeadOption(extractor: Extractor[ResultSet, _]): Option[extractor.SingleResult] =
      session.executeSelect(select.what(extractor.columns))(row => extractor.extractHeadOption(row))

    def extractAll(extractor: Extractor[ResultSet, _]): List[extractor.SingleResult] =
      session.executeSelect(select.what(extractor.columns))(row => extractor.extractAll(row))

    private implicit def resultSetIterable(resultSet: ResultSet): Iterable[ResultSet] = ResultSetIterable(resultSet)
  }

  implicit class InsertExecutorOps(insert: Insert) {
    def execute(implicit transaction: Transaction): Int = transaction.executeCommand(insert)
  }

  implicit class UpdateExecutorOps(update: Update) {
    def execute(implicit transaction: Transaction): Int = transaction.executeCommand(update)
  }

  implicit class DeleteExecutorOps(delete: Delete) {
    def execute(implicit transaction: Transaction): Int = transaction.executeCommand(delete)
  }

  implicit class BatchExecutorOps(batchCommands: Seq[Command]) {
    def executeBatch(implicit transaction: Transaction): List[Int] = transaction.executeBatch(batchCommands)
  }
}
