package li.cil.oc.common.audio.synth.operator

import li.cil.oc.common.audio.synth.{Resettable, Synth}
import li.cil.oc.common.audio.synth.channel.{Channel, ChannelId}
import li.cil.oc.common.audio.synth.parameter.Parameter

class FmOperator(channel: Channel) extends Operator {
  class Modulation extends Resettable {
    object ModIndex extends Parameter(0f, Some(channel)) {
      override def low: Float = -10000f

      override def high: Float = 10000f
    }

    override def reset(): Unit = {
      ModIndex.reset()
    }
  }

  override protected type Input = Modulation

  override def boundChannel: Channel = channel

  def addInput(channelId: ChannelId): Modulation = {
    val modulation = new Modulation
    super.addInput(channelId, modulation)

    modulation
  }

  override def removeInput(channelId: ChannelId): Unit = {
    super.removeInput(channelId)
  }

  override def process(samples: Int): Unit = {
    super.process(samples)

    for (i <- 0 until samples) {
      var out = 0f

      for ((channelId, input) <- inputChannels; inputChannel <- channel.synth.channelById(channelId)) {
        out += inputChannel.output.getAt(i) * input.ModIndex.update()
      }

      output.put(out)
    }
  }
}
