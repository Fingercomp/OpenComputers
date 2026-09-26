package li.cil.oc.common.audio.synth.filter

import li.cil.oc.common.audio.synth.Resettable
import li.cil.oc.common.audio.synth.channel.Channel
import li.cil.oc.common.audio.synth.parameter.Parameter

import java.nio.FloatBuffer

trait Filter extends Resettable {
  def process(input: FloatBuffer, output: FloatBuffer): Unit
}

object Filter {
  class Cutoff(initialSourceValue: Float, channel: Channel) extends Parameter(initialSourceValue, Some(channel)) {
    override def low: Float = 20f

    override def high: Float = 20000f

    override def curve: Parameter.Curve = Parameter.Curve.Exponential
  }

  class Resonance(channel: Channel) extends Parameter(0f, Some(channel)) {
    override def low: Float = 0f

    override def high: Float = 1f

    override def curve: Parameter.Curve = Parameter.Curve.Quadratic
  }

  class Mix(initial: Float, channel: Channel) extends Parameter(initial, Some(channel)) {
    override def low: Float = 0f

    override def high: Float = 1f
  }
}
