/*
 * Copyright (c) 2011-15 Miles Sabin
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

package shapeless.ops

import shapeless._
import shapeless.labelled.FieldType

object maps {

  /**
   * Type class supporting type safe conversion of Map to Records.
   *
   *
   */
  trait FromMap[Out <: HList] extends Serializable {
    def apply(l: Map[Any, Any]): Either[String, Out]
  }

  /**
   * `FromMap` type class instances.
   *
   *
   */
  object FromMap {

    def apply[Out <: HList](implicit from: FromMap[Out]) = from

    implicit def hnilFromMap[T] = new FromMap[HNil] {

      override def apply(l: Map[Any, Any]) = Right(HNil)
    }

    import labelled._

    implicit def hlistNesstedFromMap[K, V <: HList, OutT <: HList](implicit flt: FromMap[OutT],
                                                                   fm: FromMap[V],
                                                                   wit: Witness.Aux[K]) = new
        FromMap[FieldType[K, V] :: OutT] {

      def apply(mm: Map[Any, Any]): Either[String, FieldType[K, V] :: OutT] = {

        val key = wit.value
        import labelled._
        val value = mm.get(key)
        if (value.isEmpty) {
          Left(s"No entry of ${key} in ${mm}")
        } else {
          value.get match {
            case x: Map[_, _] =>

              for {h <- fm(x.asInstanceOf[Map[Any, Any]]).right
                   t <- flt(mm).right} yield field[K](h) :: t
            case x: Any =>
              Left(s"Expected Map[Any,Any] got ${x.getClass}")
          }
        }
      }
    }
  }

  implicit def hlistFromMap[K, V, OutT <: HList](implicit flt: FromMap[OutT], oc: Typeable[V],
                                                 wit: Witness.Aux[K]) = new
      FromMap[FieldType[K, V] :: OutT] {

    import labelled._

    def apply(mm: Map[Any, Any]): Either[String, FieldType[K, V] :: OutT] = {

      val key = wit.value


      val value = mm.get(key)
      if (value.isEmpty) {
        Left(s"No entry of ${key} in ${mm}")
      } else {
        val typed = oc.cast(value.get)
        if (typed.isEmpty) {
          Left(
            s"Can't convert ${value.get} of class ${value.get.getClass.getName} to ${oc.describe}")
        } else {
          flt(mm).right.map(t => (field[K](typed.get)) :: t)
        }
      }
    }
  }
}