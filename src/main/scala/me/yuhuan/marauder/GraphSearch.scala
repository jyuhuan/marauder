package me.yuhuan.marauder

import scala.collection._
import me.yuhuan.util.default

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */



object GraphSearch {
  //  def dfs[S](start: S, isGoal: S ⇒ Boolean)(implicit S: StateSpace[S]): Unit = {
  //    val fringe = mutable.Stack(SearchNode[S](start, SearchNode.Dummy))
  //    val explored = mutable.HashSet[S]()
  //
  //    var success = false
  //
  //    while (fringe.nonEmpty && !success) {
  //      val cur = fringe.pop()
  //      if (!(explored contains cur.state)) {
  //        explored += cur.state
  //        fringe pushAll S.succ(cur.state).map(s ⇒ SearchNode(s, cur))
  //      }
  //    }
  //  }

  def dfsa[S, A](start: S, isGoal: S ⇒ Boolean)(implicit S: StateSpaceWithAction[S, A]): Path[S, A] =
    depthFirstWithAction(start, isGoal)(S)


  def depthFirstWithAction[S, A](start: S, isGoal: S ⇒ Boolean)(implicit S: StateSpaceWithAction[S, A]): Path[S, A] = {

    val fringe = mutable.ArrayStack(SearchNodeWithAction[S, A](start, SearchNodeWithAction.Dummy, default[A]))
    val explored = mutable.HashSet[S]()

    var success = false
    var cur: SearchNodeWithAction[S, A] = null

    while (fringe.nonEmpty && !success) {
      cur = fringe.pop()

      if (isGoal(cur.state)) success = true
      else {

        if (!(explored contains cur.state)) {
          explored += cur.state

          for (a ← S.actions) {
            fringe ++= S.succ(cur.state, a).map(s ⇒ SearchNodeWithAction(s, cur, a))
          }
        }
      }
    }

    val states = mutable.ArrayBuffer[S]()
    val actions = mutable.ArrayBuffer[A]()

    var parent = cur
    while (parent != SearchNodeWithAction.Dummy) {
      states += parent.state
      actions += parent.action

      parent = parent.parent
    }

    Path(states.reverse, actions.init.reverse)
  }
}
