package decaf.frontend.util

import scala.util.parsing.combinator.Parsers

trait TupleSeq extends Parsers {
  // A ~ can be used just like normal tuples!
  implicit def tupleSeq2[T1, T2, O](f: (T1, T2) => O): (T1 ~ T2) => O = {
    case t1 ~ t2 => f(t1, t2)
  }

  // `{ case ? => ? }` is a syntactic sugar for `x match { case ? => ? }`

  implicit def tupleSeq3[T1, T2, T3, O](f: (T1, T2, T3) => O): (T1 ~ T2 ~ T3) => O = {
    case t1 ~ t2 ~ t3 => f(t1, t2, t3)
  }

  implicit def tupleSeq4[T1, T2, T3, T4, O](f: (T1, T2, T3, T4) => O): (T1 ~ T2 ~ T3 ~ T4) => O = {
    case t1 ~ t2 ~ t3 ~ t4 => f(t1, t2, t3, t4)
  }

  implicit def tupleSeq5[T1, T2, T3, T4, T5, O](f: (T1, T2, T3, T4, T5) => O): (T1 ~ T2 ~ T3 ~ T4 ~ T5) => O = {
    case t1 ~ t2 ~ t3 ~ t4 ~ t5 => f(t1, t2, t3, t4, t5)
  }

  implicit def tupleSeq6[T1, T2, T3, T4, T5, T6, O](f: (T1, T2, T3, T4, T5, T6) => O): (T1 ~ T2 ~ T3 ~ T4 ~ T5 ~ T6) => O = {
    case t1 ~ t2 ~ t3 ~ t4 ~ t5 ~ t6 => f(t1, t2, t3, t4, t5, t6)
  }
}
