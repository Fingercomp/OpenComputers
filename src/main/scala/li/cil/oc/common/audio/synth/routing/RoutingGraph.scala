package li.cil.oc.common.audio.synth.routing

import li.cil.oc.common.audio.synth.Resettable
import li.cil.oc.common.audio.synth.channel.ChannelId
import li.cil.oc.common.audio.synth.routing.RoutingGraph.Send

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class RoutingGraph(voiceChannelCount: Int, lfoChannelCount: Int) extends Resettable {
  // invariants:
  // 1. these two maps are inverses of each other:
  //    - send ∈ sendsTo(ch)      ⇔ send ∈ receivesFrom(send.targetId)
  //    - send ∈ receivesFrom(ch) ⇔ send ∈ sendsTo(ch.sourceId)
  // 2. the send graph is acyclic.
  private val _sendsTo = new mutable.HashMap[ChannelId, ArrayBuffer[Send]]()
  private val _receivesFrom = new mutable.HashMap[ChannelId, ArrayBuffer[Send]]()

  private def sendsTo(sourceId: ChannelId): ArrayBuffer[Send] = {
    _sendsTo.getOrElseUpdate(sourceId, ArrayBuffer.empty)
  }

  private def receivesFrom(targetId: ChannelId): ArrayBuffer[Send] = {
    _receivesFrom.getOrElseUpdate(targetId, ArrayBuffer.empty)
  }

  def add(sourceId: ChannelId, targetId: ChannelId, target: AnyRef): Boolean = {
    if (reachable(targetId, sourceId)) {
      false
    } else {
      val send = Send(sourceId, targetId, target)
      sendsTo(sourceId) += send
      receivesFrom(targetId) += send
      cachedToposort = None

      true
    }
  }

  def remove(sourceId: ChannelId, targetId: ChannelId, target: AnyRef): Boolean = {
    val send = Send(sourceId, targetId, target)
    var found = false

    for (sends <- _sendsTo.get(sourceId)) {
      val idx = sends.indexOf(send)

      if (idx >= 0) {
        found = true
        swapRemove(sends, idx)

        val recvs = _receivesFrom(send.targetId)
        swapRemove(recvs, recvs.indexOf(send))
        cachedToposort = None
      }
    }

    found
  }

  def reset(): Unit = {
    super.reset()
    _sendsTo.clear()
    _receivesFrom.clear()
    cachedToposort = None
  }

  def hasSends(sourceId: ChannelId): Boolean = {
    _sendsTo.get(sourceId).exists(_.nonEmpty)
  }

  def hasSends(sourceId: ChannelId, targetId: ChannelId): Boolean = {
    _sendsTo.get(sourceId) match {
      case Some(sends) => sends.exists(_.targetId == targetId)
      case None => false
    }
  }

  private var cachedToposort: Option[Array[ChannelId]] = None

  def toposort: Array[ChannelId] = {
    case class Task(sourceId: ChannelId, var sends: Iterator[Send])

    cachedToposort match {
      case Some(result) => result

      case None =>
        // the result does not actually include the master channel.
        val result = Array.fill[ChannelId](voiceChannelCount + lfoChannelCount)(ChannelId.Master)
        var idx = result.length - 1

        val discovered = mutable.HashSet.empty[ChannelId]
        val tasks = mutable.Stack.empty[Task]

        def pushTask(sourceId: ChannelId): Boolean = {
          if (sourceId == ChannelId.Master) {
            return false
          }

          if (!discovered.add(sourceId)) {
            return false
          }

          for (sends <- _sendsTo.get(sourceId)) {
            tasks += Task(sourceId, sends.iterator)
          }

          true
        }

        val voiceChannelIds = (0 until voiceChannelCount).iterator.map(ChannelId.Voice)
        val lfoChannelIds = (0 until lfoChannelCount).iterator.map(ChannelId.Lfo)

        for (root <- voiceChannelIds ++ lfoChannelIds if pushTask(root)) {
          while (tasks.nonEmpty) {
            val task = tasks.last

            if (task.sends.hasNext) {
              pushTask(task.sends.next().targetId)
            } else {
              result(idx) = task.sourceId
              idx -= 1
              tasks.pop()
            }
          }
        }

        cachedToposort = Some(result)
        result
    }
  }

  private def swapRemove[T](buf: ArrayBuffer[T], idx: Int): Unit = {
    buf(idx) = buf.last
    buf.remove(buf.length - 1)
  }

  private def reachable(from: ChannelId, to: ChannelId): Boolean = {
    if (from == to) {
      return true
    }

    val discovered = mutable.HashSet(from)
    val worklist = mutable.Stack(from)

    while (worklist.nonEmpty) {
      val sourceId = worklist.pop()
      var found = false

      for (sends <- _sendsTo.get(sourceId)) {
        for (send <- sends if discovered.add(send.targetId)) {
          if (send.targetId == to) {
            found = true
          }

          worklist.push(send.targetId)
        }
      }

      if (found) {
        return true
      }
    }

    false
  }
}

object RoutingGraph {
  private case class Send(sourceId: ChannelId, targetId: ChannelId, target: AnyRef) {
    override def equals(obj: Any): Boolean = super.equals(obj)
  }
}
