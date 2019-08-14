package decaf.frontend.tac

import scala.collection.mutable

class Temp protected(val id: Int, val size: Int) {
  override def equals(obj: Any): Boolean = obj match {
    case t: Temp => id == t.id
    case _ => false
  }

  override def toString: String = "_T" + id
}

object Temp {
  private var count: Int = 0

  private[tac] def next: Int = {
    count += 1
    count
  }

  def fresh: Temp = new Temp(next, 4)
}

class ConstTemp(override val id: Int, val value: Int) extends Temp(id, 4) {
  override def toString: String = value.toString
}

object ConstTemp {
  private val pool: mutable.HashMap[Int, ConstTemp] = new mutable.HashMap

  def apply(value: Int): ConstTemp = pool.get(value) match {
    case Some(t) => t
    case None =>
      val t = new ConstTemp(Temp.next, value)
      pool(value) = t
      t
  }
}
