package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.Synth

class LfoChannel(synth: Synth, id: ChannelId) extends Channel(synth, id) with OscillatorChannel {
  override protected def onOpen(): Unit = ???

  override protected def onClose(): Unit = ???

  override def process(samples: Int): Unit = ???

  override def reset(): Unit = ???
}
