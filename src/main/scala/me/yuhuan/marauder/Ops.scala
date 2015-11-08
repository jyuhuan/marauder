package me.yuhuan.marauder

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
object Ops {

  implicit class AnythingCanBeSearched[S](val x: S) extends AnyVal {
    def ~~>[A](implicit S: StateSpaceWithAction[S, A]) = ???
  }

}
