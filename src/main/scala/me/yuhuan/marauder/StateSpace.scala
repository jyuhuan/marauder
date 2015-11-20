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

trait StateSpaceWithLazyAction[S, A] extends StateSpace[S] {
  def succ(s: S, a: A): Seq[S]
  def succAction(s: S): Seq[A]
  def succ(s: S): scala.Seq[S] = succAction(s).flatMap(a ⇒ succ(s, a))
}