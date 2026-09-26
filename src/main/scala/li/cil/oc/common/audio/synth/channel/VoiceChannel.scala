package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.Synth
import li.cil.oc.common.audio.synth.parameter.Parameter
import li.cil.oc.common.audio.synth.waveshaper.{ShapingFunction, Waveshaper}

class VoiceChannel(synth: Synth, id: ChannelId) extends Channel(synth, id) with OscillatorChannel with FilterChannel {
  val waveshaper: Waveshaper = new Waveshaper(Some(this))

  object MasterSendLevel extends Parameter(1f, Some(this)) {
    override def low: Float = 0f

    override def high: Float = 1f
  }

  override protected def onOpen(): Unit = ???

  override protected def onClose(): Unit = ???

  override def process(samples: Int): Unit = ???

  override def reset(): Unit = ???
}
