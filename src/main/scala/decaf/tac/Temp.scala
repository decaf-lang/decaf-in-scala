package decaf.tac

import scala.collection.mutable

/**
  * A temp behaves like a register: it stores a 4 byte (i.e. 32 bits) value, and we can use load/store instructions
  * to read/write. To be concise, these registers are pseudo -- we can have as many as registers as we wish!
  *
  * @param id an increasing counter as ID
  */
class Temp protected(val id: Int) {
  /**
    * Human-readable name.
    */
  val name: String = "_T" + id

  override def equals(obj: Any): Boolean = obj match {
    case t: Temp => id == t.id
    case _ => false
  }

  override def toString: String = name
}

object Temp {
  private var count: Int = 0

  private[tac] def next: Int = {
    count += 1
    count
  }

  def fresh: Temp = new Temp(next)
}

/**
  * A constant temp is a special temp such that it stores a constant integer `value`. To save IDs, constant temps
  * are maintained internally using a constant pool (see below), and if two constant temps have the same value, then
  * they are regarded the same and their IDs are identical.
  *
  * @param id    an increasing counter as ID
  * @param value the constant integer
  */
class ConstTemp(override val id: Int, val value: Int) extends Temp(id) {
  override val name: String = value.toString
}

object ConstTemp {
  private val pool: mutable.HashMap[Int, ConstTemp] = new mutable.HashMap

  def apply(value: Int): ConstTemp = pool.get(value) match {
    case Some(t) => t // reuse the previous one
    case None => // create a new one
      val t = new ConstTemp(Temp.next, value)
      pool(value) = t
      t
  }
}
