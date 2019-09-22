package decaf.driver.error

object RuntimeError {
  final val ARRAY_INDEX_OUT_OF_BOUND = "Decaf runtime error: Array subscript out of bounds\n"

  final val NEGATIVE_ARR_SIZE = "Decaf runtime error: Cannot create negative-sized array\n"

  final val CLASS_CAST_ERROR1 = "Decaf runtime error: "

  final val CLASS_CAST_ERROR2 = " cannot be cast to "

  final val CLASS_CAST_ERROR3 = "\n"
}
