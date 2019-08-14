package decaf.frontend.tac

class Label private(val id: Int, val name: String, target: Boolean) {
  override def toString: String = name
}

object Label {
  private var count: Int = 0

  private def next: Int = {
    count += 1
    count
  }

  def fresh(): Label = fresh(false)

  def fresh(target: Boolean): Label = new Label(next, "_L" + count, target)

  def fresh(name: String, target: Boolean = false): Label = new Label(next, name, target)
}
