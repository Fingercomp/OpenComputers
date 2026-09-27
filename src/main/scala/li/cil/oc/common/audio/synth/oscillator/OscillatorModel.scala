package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.common.audio.synth.SampleBuffer
import li.cil.oc.common.audio.synth.channel.{Channel, OscillatorChannel}
import li.cil.oc.common.audio.synth.oscillator.WavetableOscillator.Wavetable

/** External properties common to all oscillators. */
trait OscillatorModel {
  /** The channel this oscillator belongs to. */
  def channel: Channel

  /** The frequency of this oscillator. */
  def frequency: Float

  /** The global wavetable. */
  def wavetable: Wavetable

  /**
   * The hard sync rate parameter.
   *
   * Must be updated by the oscillator even if it's otherwise unused.
   */
  def Sync: Oscillator.Sync

  /** The frequency modulation input. */
  def fm: Option[SampleBuffer]
}

object OscillatorModel {
  trait Listener {
    /** Called whenever the frequency changes in the model. */
    def onFrequencyChanged(freq: Float): Unit

    /** Called whenever the wavetable changes in the model. */
    def onWavetableChanged(wavetable: Wavetable): Unit = {}
  }
}
