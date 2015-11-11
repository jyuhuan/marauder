package me.yuhuan.marauder

import scala.collection.mutable

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait Path[+S, +A] { outer ⇒
  def states: IndexedSeq[S]
  def actions: IndexedSeq[A]

  def length = actions.length

  def mapState[S1](f: S ⇒ S1) = new Path[S1, A] {
    def states: IndexedSeq[S1] = outer.states.map(f)
    def actions: IndexedSeq[A] = outer.actions
  }

  def mapAction[A1](f: A ⇒ A1) = new Path[S, A1] {
    def states: IndexedSeq[S] = outer.states
    def actions: IndexedSeq[A1] = outer.actions.map(f)
  }

  def compressed: Path[S, A] = {
    val newStates = mutable.ArrayBuffer[S]()
    val newActions = mutable.ArrayBuffer[A]()

    var lastState: S = states(0)
    newStates += lastState

    for (i ← 1 until states.length) {
      val curState = states(i)
      val curAction = actions(i - 1)

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
    sb append states(0)
    for (i ← 1 until states.length) {
      sb append actions(i - 1)
      sb append states(i)
    }
    sb.toString
  }
}

object Path {
  def apply[S, A](_states: IndexedSeq[S], _actions: IndexedSeq[A]) = {
    require(_states.length == _actions.length + 1)
    new Path[S, A] {
      def states: IndexedSeq[S] = _states
      def actions: IndexedSeq[A] = _actions
    }
  }
}

object Test {
  def main(args: Array[String]) {
    val p = new Path[String, Int] {
      def states: IndexedSeq[String] = Array("NP", "VP", "VP", "VP", "ADJP", "NNP", "NNP", "NNP", "SBAR")
      def actions: IndexedSeq[Int] = Array(0, 1, 2, 3, 4, 5, 6, 7)
    }

    val pc = p.compressed

    val bp = 0
  }
}