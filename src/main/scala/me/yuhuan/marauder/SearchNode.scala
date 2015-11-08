package me.yuhuan.marauder

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait SearchNode[+S] {
  def state: S
  def parent: SearchNode[S]
}

object SearchNode {
  def apply[S](_state: S, _parent: SearchNode[S]) = new SearchNode[S] {
    def state: S = _state
    def parent: SearchNode[S] = _parent
  }

  object Dummy extends SearchNode[Nothing] {
    def state: Nothing = throw new Exception("Accessing dummy node!")
    def parent: SearchNode[Nothing] = throw new Exception("Accessing dummy node!")
  }
}

trait SearchNodeWithAction[+S, +A] extends SearchNode[S] {
  def action: A
  override def parent: SearchNodeWithAction[S, A]
}

object SearchNodeWithAction {
  def apply[S, A](_state: S, _parent: SearchNodeWithAction[S, A], _action: A) = new SearchNodeWithAction[S, A] {
    def state: S = _state
    def parent: SearchNodeWithAction[S, A] = _parent
    def action: A = _action
  }
  object Dummy extends SearchNodeWithAction[Nothing, Nothing] {
    def state: Nothing = throw new Exception("Accessing dummy node!")
    def parent: SearchNodeWithAction[Nothing, Nothing] = throw new Exception("Accessing dummy node!")
    def action: Nothing = throw new Exception("Accessing dummy node!")
  }
}