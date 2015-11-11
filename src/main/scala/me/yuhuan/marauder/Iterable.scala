package me.yuhuan.marauder

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
object Iterable {
  def depthFirst[S](s: S)(implicit S: StateSpace[S]): Iterable[S] = new scala.collection.Iterable[S] {
    def iterator: Iterator[S] = Iterator.depthFirst(s)(S)
  }

}
