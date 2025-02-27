package util

import collection.mutable.{Map => MMap}

object Cache {
  def apply[A, B](): Cache[A, B] = Cache(MMap[A, B]())
}

case class Cache[A, B](content: MMap[A, B]) {
  def get(a: A): Option[B] = content.get(a)

  def put(a: A, b: B): Unit = content(a) = b

  def getOrPut(a: A, f: A => B): B = {
    if (content.contains(a)) {
      content(a)
    } else {
      put(a, f(a))
      content(a)
    }
  }
}
