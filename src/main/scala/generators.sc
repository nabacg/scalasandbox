import java.util.Random

val rand = new Random

rand.nextInt()

trait Generator[+T] {
  self =>
  def generate : T

  def map[S](f: T => S) : Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]) : Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new Random
  def generate = rand.nextInt()
}

var booleans = new Generator[Boolean] {
  def generate = integers.generate > 0
}

