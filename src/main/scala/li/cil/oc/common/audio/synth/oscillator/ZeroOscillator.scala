package li.cil.oc.common.audio.synth.oscillator

class ZeroOscillator(model: OscillatorModel) extends Oscillator(model) {
  override def onFrequencyChanged(freq: Float): Unit = {}

  override def process(samples: Int): Unit = {
    for (_ <- 0 until samples) {
      output.put(0f)
      model.Sync.update()
    }
  }

  override def reset(): Unit = {
    super.reset()
  }
}
