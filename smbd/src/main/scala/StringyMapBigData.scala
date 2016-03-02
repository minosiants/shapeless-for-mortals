// Copyright (C) 2015 Sam Halliday
// License: http://www.apache.org/licenses/LICENSE-2.0
/**
 * TypeClass (api/impl/syntax) for marshalling objects into
 * `java.util.HashMap<String,Object>` (yay, big data!).
 */
package s4m.smbd

import shapeless._, labelled._
//import spray.json.{JsObject, JsValue}
import scala.collection.JavaConverters._
/**
 * This exercise involves writing tests, only a skeleton is provided.
 *
 * - Exercise 1.1: derive =BigDataFormat= for sealed traits.
 * - Exercise 1.2: define identity constraints using singleton types.
 */
package object api {
  type StringyMap = java.util.HashMap[String, AnyRef]
  type BigResult[T] = Either[String, T] // aggregating errors doesn't add much
}

package api {
  trait BigDataFormat[T] {
    def label: String
    def toProperties(t: T): StringyMap
    def fromProperties(m: StringyMap): BigResult[T]
  }

  trait SPrimitive[V] {
    def toValue(v: V): AnyRef
    def fromValue(v: AnyRef): V
  }

  // EXERCISE 1.2
  trait BigDataFormatId[T, V] {
    def key: String
    def value(t: T): V
  }

  object BigDataFormat {
    implicit class RichStringyMap(val props: StringyMap) extends AnyVal {
      def :+(kv: (String, AnyRef)): StringyMap = {
        props.put(kv._1, kv._2)
        props
      }
    }
  }
}

package object impl {
  import api._
  import api.BigDataFormat._
  // EXERCISE 1.1 goes here


  // EXERCISE 1.1 goes here
  implicit object IntSPrimitive extends SPrimitive[Int] {
    def toValue(v: Int): AnyRef = Integer.valueOf(v)
    def fromValue(v: AnyRef): Int = v.asInstanceOf[Int]
  }
  implicit object StringSPrimitive extends SPrimitive[String] {
    def toValue(v: String): AnyRef = v
    def fromValue(v: AnyRef): String = v.asInstanceOf[String]
  }
  implicit object BooleanSPrimitive extends SPrimitive[Boolean] {
    def toValue(v: Boolean): AnyRef = java.lang.Boolean.valueOf(v)
    def fromValue(v: AnyRef): Boolean = v.asInstanceOf[Boolean]
  }
  implicit object DoubleSPrimitive extends SPrimitive[Double] {
    def toValue(v: Double): AnyRef = java.lang.Double.valueOf(v)
    def fromValue(v: AnyRef): Double = v.asInstanceOf[Double]
  }
   implicit def hNilBigDataFormat = new BigDataFormat[HNil] {

    def label: String = "EMPTY"

    def toProperties(t: HNil): StringyMap = new StringyMap()

    def fromProperties(m: StringyMap): BigResult[HNil] = Right(HNil)
  }

  implicit def hListBigDataFormat[Key <:Symbol, Value, Remaining <:HList](
    implicit
    key:Witness.Aux[Key],
    sp: SPrimitive[Value],
    lazybdft: Lazy[BigDataFormat[Remaining]]
  ):BigDataFormat[FieldType[Key, Value]::Remaining] = new BigDataFormat[FieldType[Key, Value]::Remaining] {
    val bdft = lazybdft.value
    def label: String = key.value.name

    def toProperties(hlist: FieldType[Key, Value]:: Remaining): StringyMap = bdft.toProperties(hlist.tail) :+
      (label -> sp.toValue(hlist.head))

    def fromProperties(m: StringyMap): BigResult[FieldType[Key, Value]::Remaining] = {
      (m.get(label) match {
        case null => Left("some sensible error here")
        case value => Right(sp.fromValue(value))
      }).right.flatMap{ head =>
        bdft.fromProperties(m).right.map{ tail =>
          field[Key](head)::tail
        }
      }
    }
  }

  implicit def cNilBigDataFormat = new BigDataFormat[CNil] {
    override def label: String = throw new RuntimeException

    override def toProperties(t: CNil): StringyMap = throw new RuntimeException

    override def fromProperties(m: StringyMap): BigResult[CNil] = throw new RuntimeException
  }


  implicit def coproductBigDataFormat[Name <: Symbol, Head, Tail <: Coproduct ](
    implicit
    key: Witness.Aux[Name],
    lazyBdfh:Lazy[BigDataFormat[Head]],
    lazyBdft:Lazy[BigDataFormat[Tail]]
  ):BigDataFormat[FieldType[Name, Head] :+: Tail] = new BigDataFormat[FieldType[Name, Head] :+: Tail] {

    val bdfh = lazyBdfh.value
    val bdft = lazyBdft.value

    def toProperties(lr: FieldType[Name, Head] :+: Tail): StringyMap = lr match {
      case Inl(head) => bdfh.toProperties(head) :+ ("_type" -> label)
      case Inr(tail) => bdft.toProperties(tail)
    }
    def fromProperties(m: StringyMap): BigResult[FieldType[Name, Head]:+:Tail] = {
      if(m.get("_type").asInstanceOf[String] == label)
        bdfh.fromProperties(m).right.map(x => Inl(field[Name](x)))
      else
        bdft.fromProperties(m).right.map(x => Inr(x))
    }

    def label: String = key.value.name
  }

  implicit def familyBigDataFormat[T, Repr] (
    implicit
    gen:LabelledGeneric.Aux[T, Repr],
    lazyBdf:Lazy[BigDataFormat[Repr]],
    tpe: Typeable[T]
  ):BigDataFormat[T]= new BigDataFormat[T]{
    val bdf = lazyBdf.value
    override def label: String = bdf.label

    override def toProperties(t: T): StringyMap = bdf.toProperties(gen.to(t))

    override def fromProperties(m: StringyMap): BigResult[T] = bdf.fromProperties(m) match {
      case Left(e) => Left(e)
      case Right(v) => Right(gen.from(v))
    }
  }
}

package impl {
  import api._

  // EXERCISE 1.2 goes here
}

package object syntax {
  import api._

  implicit class RichBigResult[R](val e: BigResult[R]) extends AnyVal {
    def getOrThrowError: R = e match {
      case Left(error) => throw new IllegalArgumentException(error.mkString(","))
      case Right(r) => r
    }
  }

  /** Syntactic helper for serialisables. */
  implicit class RichBigDataFormat[T](val t: T) extends AnyVal {
    def label(implicit s: BigDataFormat[T]): String = s.label
    def toProperties(implicit s: BigDataFormat[T]): StringyMap = s.toProperties(t)
    def idKey[P](implicit lens: Lens[T, P]): String = ???
    def idValue[P](implicit lens: Lens[T, P]): P = lens.get(t)
  }

  implicit class RichProperties(val props: StringyMap) extends AnyVal {
    def as[T](implicit s: BigDataFormat[T]): T = s.fromProperties(props).getOrThrowError

  }
}
