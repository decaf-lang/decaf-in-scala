package decaf.tac

/**
  * Labels, for jump instructions. Specific program locations and all procedures/functions have labels.
  *
  * @param id     an increasing counter as ID
  * @param name   human-readable name
  * @param target TODO
  */
class Label private(val id: Int, val name: String, target: Boolean) {
  override def toString: String = name
}

object Label {
  private var count: Int = 0

  private def next: Int = {
    count += 1
    count
  }

  /**
    * Create a new label.
    *
    * @return the new label
    */
  def fresh(): Label = fresh(false)

  /**
    * Create a new label.
    *
    * @param target
    * @return the new label
    */
  def fresh(target: Boolean): Label = new Label(next, "_L" + count, target)

  /**
    * Create a new label and specify its name.
    *
    * @param name the name
    * @param target
    * @return the new label
    */
  def fresh(name: String, target: Boolean = false): Label = new Label(next, name, target)
}
