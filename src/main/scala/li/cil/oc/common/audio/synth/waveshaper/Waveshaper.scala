package li.cil.oc.common.audio.synth.waveshaper

import li.cil.oc.common.audio.synth.channel.Channel
import li.cil.oc.common.audio.synth.filter.PerSampleFilter
import li.cil.oc.common.audio.synth.parameter.Parameter

class Waveshaper(channel: Option[Channel]) extends PerSampleFilter {
  var shaper: ShapingFunction = ShapingFunction.Identity

  object Drive extends Parameter(0, channel) {
    override def low: Float = 1f / 256

    override def high: Float = 256f

    override def curve: Parameter.Curve = Parameter.Curve.Exponential
  }

  override def reset(): Unit = {
    super.reset()
    shaper = ShapingFunction.Identity
    Drive.reset()
  }

  def processSample(input: Float): Float = {
    shaper.shape(input, Drive.update())
  }
}
