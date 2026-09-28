package li.cil.oc.common.audio.synth.operator

import li.cil.oc.common.audio.synth.Resettable
import li.cil.oc.common.audio.synth.channel.{Channel, ChannelId}
import li.cil.oc.common.audio.synth.parameter.Parameter

class SumOperator(channel: Channel) extends Operator {
  class Modulation extends Resettable {
    object Level extends Parameter(0f, Some(channel)) {
      override def low: Float = -1f

      override def high: Float = 1f
    }

    override def reset(): Unit = {
      Level.reset()
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

      for (channelId <- inputChannels.keys; inputChannel <- channel.synth.channelById(channelId)) {
        out += inputChannel.output.getAt(i)
      }

      output.put(out)
    }
  }
}
