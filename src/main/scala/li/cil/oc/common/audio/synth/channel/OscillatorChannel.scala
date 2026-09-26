package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.oscillator.{Oscillator, OscillatorModel, ZeroOscillator}
import li.cil.oc.common.audio.synth.parameter.Parameter

/** A channel with an oscillator. */
trait OscillatorChannel extends Channel {
  private var _frequency: Float = 0f

  def frequency: Float = _frequency

  def frequency_=(freq: Float): Unit = {
    if (freq != _frequency) {
      _frequency = freq
      _oscillator.onFrequencyChanged(freq)
    }
  }

  private var _oscillator: Oscillator = new ZeroOscillator(OscillatorModel(this))

  def oscillator: Oscillator = _oscillator

  def oscillator_=(oscillator: Oscillator): Unit = {
    if (oscillator ne _oscillator) {
      _oscillator.reset()
      _oscillator = oscillator
    }
  }

  object OscillatorLevel extends Parameter(1f, Some(this)) {
    override def low: Float = 0f

    override def high: Float = 1f
  }

  override def reset(): Unit = {
    super.reset()
    frequency = 0f
  }
}
