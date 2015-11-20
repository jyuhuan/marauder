package me.yuhuan.marauder

import scala.collection._
import me.yuhuan.util.default

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */



object GraphSearch {
  def depthFrist[S](start: S, isGoal: S ⇒ Boolean)(implicit S: StateSpace[S]): Seq[S] = {
    val fringe = mutable.ArrayStack(SearchNode[S](start, SearchNode.Dummy))
    val explored = mutable.HashSet[S]()

    var success = false
    var cur: SearchNode[S] = null

    while (fringe.nonEmpty && !success) {
      cur = fringe.pop()

      if (isGoal(cur.state)) success = true
      else {
        if (!(explored contains cur.state)) {
          explored += cur.state
          fringe ++= S.succ(cur.state).map(s ⇒ SearchNode(s, cur)).reverse
        }
      }
    }

    val history = mutable.ArrayBuffer[S]()
    var parent = cur
    while (parent != SearchNode.Dummy) {
      history += parent.state
      parent = parent.parent
    }

    history.reverse
  }

  def breadthFirst[S](start: S, isGoal: S ⇒ Boolean)(implicit S: StateSpace[S]): Seq[S] = {
    val fringe = mutable.Queue(SearchNode[S](start, SearchNode.Dummy))
    val explored = mutable.HashSet[S]()

    var success = false
    var cur: SearchNode[S] = null

    while (fringe.nonEmpty && !success) {
      cur = fringe.dequeue()

      if (isGoal(cur.state)) success = true
      else {
        if (!(explored contains cur.state)) {
          explored += cur.state
          fringe ++= S.succ(cur.state).map(s ⇒ SearchNode(s, cur)).reverse
        }
      }
    }

    val history = mutable.ArrayBuffer[S]()
    var parent = cur
    while (parent != SearchNode.Dummy) {
      history += parent.state
      parent = parent.parent
    }

    history.reverse
  }

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

  def breadthFirstWithAction[S, A](start: S, isGoal: S ⇒ Boolean)(implicit S: StateSpaceWithAction[S, A]): Path[S, A] = {

    val fringe = mutable.Queue(SearchNodeWithAction[S, A](start, SearchNodeWithAction.Dummy, default[A]))
    val explored = mutable.HashSet[S]()

    var success = false
    var cur: SearchNodeWithAction[S, A] = null

    while (fringe.nonEmpty && !success) {
      cur = fringe.dequeue()

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

  def depthFirstWithLazyAction[S, A](start: S, isGoal: S => Boolean)(implicit S: StateSpaceWithLazyAction[S, A]): Path[S, A] = {
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

          for (a ← S.succAction(cur.state)) {
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

  def breadthFirstWithLazyAction[S, A](start: S, isGoal: S => Boolean)(implicit S: StateSpaceWithLazyAction[S, A]): Path[S, A] = {
    val fringe = mutable.Queue(SearchNodeWithAction[S, A](start, SearchNodeWithAction.Dummy, default[A]))
    val explored = mutable.HashSet[S]()

    var success = false
    var cur: SearchNodeWithAction[S, A] = null

    while (fringe.nonEmpty && !success) {
      cur = fringe.dequeue()

      if (isGoal(cur.state)) success = true
      else {

        if (!(explored contains cur.state)) {
          explored += cur.state

          for (a ← S.succAction(cur.state)) {
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
