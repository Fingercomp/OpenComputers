package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.Synth
import li.cil.oc.common.audio.synth.operator.{AmOperator, FmOperator, SumOperator}
import li.cil.oc.common.audio.synth.parameter.Parameter
import li.cil.oc.common.audio.synth.waveshaper.Waveshaper

class VoiceChannel(synth: Synth, id: ChannelId) extends Channel(synth, id) with OscillatorChannel with FilteredChannel {
  val fm: FmOperator = new FmOperator(this)
  val sum: SumOperator = new SumOperator(this)
  val waveshaper: Waveshaper = new Waveshaper(Some(this))

  object FeedbackGain extends Parameter(0f, Some(this)) {
    override def low: Float = -1f

    override def high: Float = 1f
  }

  val am: AmOperator = new AmOperator(this)

  object MasterSendLevel extends Parameter(1f, Some(this)) {
    override def low: Float = 0f

    override def high: Float = 1f
  }

  private var feedback = 0f

  override protected def fmOperator: Option[FmOperator] = Some(fm)

  override def reset(): Unit = {
    super.reset()
    waveshaper.reset()
    FeedbackGain.reset()
    MasterSendLevel.reset()
    feedback = 0f
  }

  override def process(samples: Int): Unit = {
    super.process(samples)

    sum.process(samples)
    am.process(samples)

    for (_ <- 0 until samples) {
      var out = oscillator.output.get() + sum.output.get() + feedback
      out = waveshaper.processSample(out)

      for (filter <- filter) {
        out = filter.processSample(out)
      }

      feedback = out * FeedbackGain.update()
      out *= 1 + am.output.get()
      output.put(out * outputGain)
    }
  }
}
