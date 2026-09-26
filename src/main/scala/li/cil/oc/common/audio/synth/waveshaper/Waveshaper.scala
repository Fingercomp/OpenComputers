package li.cil.oc.common.audio.synth.waveshaper

import li.cil.oc.common.audio.synth.channel.Channel
import li.cil.oc.common.audio.synth.filter.Filter
import li.cil.oc.common.audio.synth.parameter.Parameter

import java.nio.FloatBuffer

class Waveshaper(channel: Option[Channel]) extends Filter {
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

  def process(input: FloatBuffer, output: FloatBuffer): Unit = {
    while (input.hasRemaining) {
      output.put(shaper.shape(input.get(), Drive.update()))
    }
  }
}
