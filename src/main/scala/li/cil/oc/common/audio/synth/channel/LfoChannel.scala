package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.Synth

class LfoChannel(synth: Synth, id: ChannelId) extends Channel(synth, id) with OscillatorChannel {
  override def process(samples: Int): Unit = {
    super.process(samples)

    while (oscillator.output.nonEmpty) {
      output.put(oscillator.output.get() * outputGain)
    }
  }
}
