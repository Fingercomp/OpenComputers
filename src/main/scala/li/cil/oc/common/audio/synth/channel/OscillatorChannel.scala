package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.SampleBuffer
import li.cil.oc.common.audio.synth.envelope.AdsrEnvelope
import li.cil.oc.common.audio.synth.operator.FmOperator
import li.cil.oc.common.audio.synth.oscillator.WavetableOscillator.Wavetable
import li.cil.oc.common.audio.synth.oscillator.{Oscillator, OscillatorModel, WavetableOscillator, ZeroOscillator}
import li.cil.oc.common.audio.synth.parameter.Parameter

/** A channel with an oscillator. */
trait OscillatorChannel extends Channel { oscCh =>
  private var _frequency: Float = 0f

  def frequency: Float = _frequency

  def frequency_=(freq: Float): Unit = {
    if (freq != _frequency) {
      _frequency = freq
      _oscillator.onFrequencyChanged(freq)
    }
  }

  protected def fmOperator: Option[FmOperator]

  val Sync = new Oscillator.Sync(this)

  val oscillatorModel = new OscillatorModel {
    override def channel: Channel = oscCh

    override def frequency: Float = oscCh.frequency

    override def wavetable: Wavetable = oscCh.synth.wavetable

    override def Sync: Oscillator.Sync = oscCh.Sync

    override def fm: Option[SampleBuffer] = oscCh.fmOperator.map(_.output)
  }

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

    for (fm <- fmOperator) {
      // remove fm data from previous process() if the oscillator neglects to read it.
      fm.output.skipUnread()
      fm.process(samples)
    }

    // generate samples.
    oscillator.process(samples)

    // attenuate them according to the envelope and oscillator level.
    for (i <- 0 until samples) {
      val envLevel = envelope.update()
      oscillator.output.replace(i, oscillator.output.getAt(i) * OscillatorLevel.update() * envLevel)
    }
  }
}
