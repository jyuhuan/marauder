package me.yuhuan.marauder

import scala.collection.mutable

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
object Iterator {

  def depthFirst[S](s: S)(implicit S: StateSpace[S]) = new Iterator[S] {
    val fringe = mutable.ArrayStack(SearchNode[S](s, SearchNode.Dummy))
    val explored = mutable.HashSet[S]()
    var cur: SearchNode[S] = null

    def hasNext: Boolean = fringe.nonEmpty
    def next(): S = {
      cur = fringe.pop()

      if (!(explored contains cur.state)) {
        explored += cur.state
        fringe ++= S.succ(cur.state).map(s ⇒ SearchNode(s, cur)).reverse
      }
      cur.state
    }
  }
}
