package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.common.audio.synth.envelope.AdsrEnvelope

class AdsrOscillator(model: OscillatorModel) extends Oscillator(model) {
  private val envelope = new AdsrEnvelope(Some(model.channel))

  val Attack = envelope.Attack
  val Decay = envelope.Decay
  val Sustain = envelope.Sustain
  val Release = envelope.Release

  override def reset(): Unit = {
    envelope.reset()
  }

  override def onOpen(): Unit = {
    super.onOpen()
    envelope.onOpen()
  }

  override def onClose(): Unit = {
    super.onClose()
    envelope.onClose()
  }

  override def onFrequencyChanged(freq: Float): Unit = {
    // ignore.
  }

  override def process(samples: Int): Unit = {
    for (_ <- 0 until samples) {
      model.Sync.update()
      output.put(envelope.update())
    }
  }
}
