package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.envelope.AdsrEnvelope
import li.cil.oc.common.audio.synth.oscillator.{Oscillator, OscillatorModel, WavetableOscillator, ZeroOscillator}
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

  val Sync = new Oscillator.Sync(this)
  val oscillatorModel = OscillatorModel(this)
  private var _oscillator: Oscillator = new ZeroOscillator(oscillatorModel)

  def oscillator: Oscillator = _oscillator

  def oscillator_=(oscillator: Oscillator): Unit = {
    if (oscillator ne _oscillator) {
      _oscillator.reset()
      _oscillator = oscillator
    }
  }

  val envelope = new AdsrEnvelope(Some(this))

  object OscillatorLevel extends Parameter(1f, Some(this)) {
    override def low: Float = 0f

    override def high: Float = 1f
  }

  override def reset(): Unit = {
    super.reset()
    frequency = 0f
    Sync.reset()
    oscillator = new ZeroOscillator(oscillatorModel)
    envelope.reset()
    OscillatorLevel.reset()
  }

  override def onWavetableChanged(wavetable: WavetableOscillator.Wavetable): Unit = {
    super.onWavetableChanged(wavetable)
    oscillator.onWavetableChanged(wavetable)
  }

  override protected def onOpen(): Unit = {
    super.onOpen()
    oscillator.onOpen()
  }

  override protected def onClose(): Unit = {
    super.onClose()
    oscillator.onClose()
  }

  override def process(samples: Int): Unit = {
    super.process(samples)

    // generate samples.
    oscillator.process(samples)

    // attenuate them according to the envelope and oscillator level.
    for (i <- 0 until samples) {
      val envLevel = envelope.update()
      oscillator.output.replace(i, oscillator.output.getAt(i) * OscillatorLevel.update() * envLevel)
    }
  }
}
