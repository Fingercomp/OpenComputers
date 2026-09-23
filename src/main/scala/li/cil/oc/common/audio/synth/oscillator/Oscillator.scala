package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.common.audio.synth.channel.Channel
import li.cil.oc.common.audio.synth.parameter.Parameter
import li.cil.oc.common.audio.synth.{ChannelBound, SignalGenerator}

trait Oscillator extends SignalGenerator with ChannelBound {
  def setFrequency(freq: Float): Unit
}

object Oscillator {
  class Sync(channel: Channel) extends Parameter(1f, Some(channel)) {
    override def low: Float = 1f

    override def high: Float = 3f

    override def curve: Parameter.Curve = Parameter.Curve.Exponential
  }
}
