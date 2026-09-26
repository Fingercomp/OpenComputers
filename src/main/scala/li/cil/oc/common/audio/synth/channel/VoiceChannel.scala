package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.Synth
import li.cil.oc.common.audio.synth.parameter.Parameter
import li.cil.oc.common.audio.synth.waveshaper.Waveshaper

class VoiceChannel(synth: Synth, id: ChannelId) extends Channel(synth, id) with OscillatorChannel with FilteredChannel {
  val waveshaper: Waveshaper = new Waveshaper(Some(this))

  object FeedbackLevel extends Parameter(0f, Some(this)) {
    override def low: Float = -1f

    override def high: Float = 1f
  }

  object MasterSendLevel extends Parameter(1f, Some(this)) {
    override def low: Float = 0f

    override def high: Float = 1f
  }

  private var feedback = 0f

  override def reset(): Unit = {
    super.reset()
    waveshaper.reset()
    FeedbackLevel.reset()
    MasterSendLevel.reset()
    feedback = 0f
  }

  override def process(samples: Int): Unit = {
    super.process(samples)

    for (_ <- 0 until samples) {
      var out = oscillator.output.get() + feedback
      out = waveshaper.processSample(out)

      for (filter <- filter) {
        out = filter.processSample(out)
      }

      feedback = out * FeedbackLevel.update()
      output.put(out * outputGain)
    }
  }
}
