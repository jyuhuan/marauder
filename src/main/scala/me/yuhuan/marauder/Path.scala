package me.yuhuan.marauder

import scala.collection.mutable

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait Path[+S, +A] { outer ⇒
  def ss: IndexedSeq[S]
  def as: IndexedSeq[A]

  def length = as.length

  def mapState[S1](f: S ⇒ S1) = new Path[S1, A] {
    def ss: IndexedSeq[S1] = outer.ss.map(f)
    def as: IndexedSeq[A] = outer.as
  }

  def mapAction[A1](f: A ⇒ A1) = new Path[S, A1] {
    def ss: IndexedSeq[S] = outer.ss
    def as: IndexedSeq[A1] = outer.as.map(f)
  }

  def compressed: Path[S, A] = {
    val newStates = mutable.ArrayBuffer[S]()
    val newActions = mutable.ArrayBuffer[A]()

    var lastState: S = ss(0)
    newStates += lastState

    for (i ← 1 until ss.length) {
      val curState = ss(i)
      val curAction = as(i - 1)

      if (curState != lastState) {
        newStates += curState
        newActions += curAction
        lastState = curState
      }
    }
    Path(newStates, newActions)
  }

  override def toString = {
    val sb = new StringBuilder()
    sb append ss(0)
    for (i ← 1 until ss.length) {
      sb append as(i - 1)
      sb append ss(i)
    }
    sb.toString
  }
}

object Path {
  def apply[S, A](states: IndexedSeq[S], actions: IndexedSeq[A]) = {
    require(states.length == actions.length + 1)
    new Path[S, A] {
      def ss: IndexedSeq[S] = states
      def as: IndexedSeq[A] = actions
    }
  }
}

object Test {
  def main(args: Array[String]) {
    val p = new Path[String, Int] {
      def ss: IndexedSeq[String] = Array("NP", "VP", "VP", "VP", "ADJP", "NNP", "NNP", "NNP", "SBAR")
      def as: IndexedSeq[Int] = Array(0, 1, 2, 3, 4, 5, 6, 7)
    }

    val pc = p.compressed

    val bp = 0
  }
}