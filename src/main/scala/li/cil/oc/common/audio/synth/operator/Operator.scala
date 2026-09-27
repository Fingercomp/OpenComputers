package li.cil.oc.common.audio.synth.operator

import li.cil.oc.common.audio.synth.channel.{ChannelBound, ChannelId}
import li.cil.oc.common.audio.synth.{Resettable, SignalGenerator}

import scala.collection.mutable

trait Operator extends SignalGenerator with ChannelBound {
  protected type Input <: Resettable

  protected val inputChannels = mutable.HashMap.empty[ChannelId, Input]

  protected def addInput(channelId: ChannelId, input: Input): Unit = {
    inputChannels.put(channelId, input) match {
      case Some(prevInput) => prevInput.reset()
      case None =>
        boundChannel.synth.addChannelSend(channelId, boundChannel.id, this)
    }
  }

  protected def removeInput(channelId: ChannelId): Unit = {
    for (prevInput <- inputChannels.remove(channelId)) {
      prevInput.reset()
      boundChannel.synth.removeChannelSend(channelId, boundChannel.id, this)
    }
  }

  override def reset(): Unit = {
    for ((channelId, input) <- inputChannels) {
      input.reset()
      boundChannel.synth.removeChannelSend(channelId, boundChannel.id, this)
    }

    inputChannels.clear()
  }
}
