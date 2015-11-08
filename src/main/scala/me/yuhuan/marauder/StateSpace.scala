package me.yuhuan.marauder

import scala.collection.Seq

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait StateSpace[S] {
  def succ(s: S): Seq[S]
}

trait StateSpaceWithAction[S, A] extends StateSpace[S] {
  def actions: Seq[A]
  def succ(s: S, a: A): Seq[S]
  def succ(s: S): scala.Seq[S] = actions.flatMap(a ⇒ succ(s, a))
}
