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

package sqlest.ast.syntax

import sqlest.ast._

trait UpdateSyntax {
  /** Update table */
  def apply(table: Table) = UpdateSetterBuilder(table)
}

case class UpdateSetterBuilder(table: Table) {
  def set(setters: Setter[_, _]*): UpdateWhereBuilder =
    UpdateWhereBuilder(this.table, setters)

  def set(setters: => Seq[Setter[_, _]]): UpdateWhereBuilder =
    UpdateWhereBuilder(this.table, setters)
}

case class UpdateWhereBuilder(table: Table, setters: Seq[Setter[_, _]]) {
  def where(expr: Column[Boolean]): Update =
    Update(table = this.table, set = setters, where = Some(expr))

  /** Creates an update statement with no where clause */
  def updateAll = Update(table = this.table, set = setters, where = None)
}
