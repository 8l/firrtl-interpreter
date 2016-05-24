// See LICENSE for license details.

package firrtl_interpreter

import scala.StringBuilder
import scala.collection.mutable


case class TimerEvent(tag: String) {
  var events  = 0L
  var seconds = 0L
}

object Timer {
  var enabled = true
  val timingLog = new mutable.HashMap[String, TimerEvent]

  def apply[R](tag: String)(block: => R): R = {
    if(enabled) {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()

      val timerEvent = timingLog.getOrElseUpdate(tag, new TimerEvent(tag))
      timerEvent.events += 1
      timerEvent.seconds += (t1 - t0)
      result
    }
    else {
      block
    }
  }

  def entryFor(tag: String): String = {
    timingLog.get(tag) match {
      case Some(entry) => s"${entry.events}:${entry.seconds}:${entry.seconds / entry.events}"
      case _           => ""
    }
  }

  def clear(): Unit = {
    timingLog.clear()
  }

  def report(): String = {
    val sortedTags = timingLog.keys.toSeq.sorted
    sortedTags.map { tag =>
      s"$tag:${entryFor(tag)}"
    }.mkString("\n")
  }
}
