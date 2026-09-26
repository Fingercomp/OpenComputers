package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.Synth

class MasterChannel(synth: Synth) extends Channel(synth, ChannelId.Master) {
  override protected def onOpen(): Unit = {}

  override protected def onClose(): Unit = {}

  override def process(samples: Int): Unit = ???

  override def reset(): Unit = ???
}
