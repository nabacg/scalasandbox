abstract class IntSet {
  def incl(x : Int ) : IntSet
  def contains (x: Int) : Boolean
}
object Empty extends IntSet {
  def contains(x: Int) : Boolean = false
  def incl(x : Int) : IntSet = new NonEmpty(x, Empty, Empty)

  override def toString()  = "."
}

class NonEmpty(elem: Int, l : IntSet, r : IntSet) extends IntSet {
  def incl(x : Int ) : IntSet =
    if (x < elem) new NonEmpty(elem, l incl x, r)
    else if (x > elem) new NonEmpty(elem, l, r incl x)
    else this
  def contains (x: Int) : Boolean =
    if (x < elem) l contains x
    else if (x > elem) r contains x
    else true
  override def toString() = {
    "{" + l.toString() +
    elem.toString() +
    r.toString() +"}"
  }
}

object NonEmpty {
  def apply(i: Int) = new NonEmpty(i, Empty, Empty)
}

val t1 = new NonEmpty(5, NonEmpty(2), Empty)
t1.contains(2)
t1.contains(3)

val t2 = t1 incl 8 incl 5
t2.contains(8)
t2.toString


