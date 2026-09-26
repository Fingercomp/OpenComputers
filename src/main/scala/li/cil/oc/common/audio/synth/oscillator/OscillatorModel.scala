package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.common.audio.synth.channel.{Channel, OscillatorChannel}
import li.cil.oc.common.audio.synth.oscillator.WavetableOscillator.Wavetable
import li.cil.oc.common.audio.synth.parameter.Parameter

/** External properties common to all oscillators. */
trait OscillatorModel {
  /** The channel this oscillator belongs to. */
  def channel: Channel

  /** The frequency of this oscillator. */
  def frequency: Float

  /** The global wavetable. */
  def wavetable: Wavetable

  object Sync extends Parameter(1f, Some(channel)) {
    override def low: Float = 1f

    override def high: Float = 3f

    override def curve: Parameter.Curve = Parameter.Curve.Exponential
  }
}

object OscillatorModel {
  trait Listener {
    /** Called whenever the frequency changes in the model. */
    def onFrequencyChanged(freq: Float): Unit

    /** Called whenever the wavetable changes in the model. */
    def onWavetableChanged(wavetable: Wavetable): Unit = {}
  }

  def apply(ch: OscillatorChannel): OscillatorModel = new OscillatorModel {
    override def channel: Channel = ch

    override def frequency: Float = ch.frequency

    override def wavetable: Wavetable = ch.synth.wavetable
  }
}
