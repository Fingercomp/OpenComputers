package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.common.audio.synth.SignalGenerator
import li.cil.oc.common.audio.synth.channel.{Channel, Triggerable}
import li.cil.oc.common.audio.synth.parameter.Parameter

abstract class Oscillator(protected var model: OscillatorModel)
  extends SignalGenerator
    with Triggerable
    with OscillatorModel.Listener

object Oscillator {
  class Sync(channel: Channel) extends Parameter(1f, Some(channel)) {
    override def low: Float = 1f

    override def high: Float = 3f

    override def curve: Parameter.Curve = Parameter.Curve.Exponential
  }
}
