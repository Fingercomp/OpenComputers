package li.cil.oc.common.audio.synth.operator

import li.cil.oc.common.audio.synth.Resettable
import li.cil.oc.common.audio.synth.channel.{Channel, ChannelId}

class SumOperator(channel: Channel) extends Operator {
  private object DummyModulation extends Resettable {
    override def reset(): Unit = {}
  }

  override protected type Input = DummyModulation.type

  override def boundChannel: Channel = channel

  def addInput(channelId: ChannelId): Unit = {
    super.addInput(channelId, DummyModulation)
  }

  override def removeInput(channelId: ChannelId): Unit = {
    super.removeInput(channelId)
  }

  override def process(samples: Int): Unit = {
    super.process(samples)

    for (i <- 0 until samples) {
      var out = 0f

      for (channelId <- inputChannels.keys) {
        out += channel.synth.channelById(channelId).output.getAt(i)
      }

      output.put(out)
    }
  }
}
