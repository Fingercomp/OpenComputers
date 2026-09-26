package li.cil.oc.common.audio.synth.routing

import li.cil.oc.common.audio.synth.Resettable
import li.cil.oc.common.audio.synth.channel.ChannelId
import li.cil.oc.common.audio.synth.routing.RoutingGraph.Send

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class RoutingGraph extends Resettable {
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
      }
    }

    found
  }

  def reset(): Unit = {
    super.reset()
    _sendsTo.clear()
    _receivesFrom.clear()
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

  private def swapRemove[T](buf: ArrayBuffer[T], idx: Int): Unit = {
    buf(idx) = buf.last
    buf.remove(buf.length - 1)
  }

  private def reachable(from: ChannelId, to: ChannelId): Boolean = {
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
