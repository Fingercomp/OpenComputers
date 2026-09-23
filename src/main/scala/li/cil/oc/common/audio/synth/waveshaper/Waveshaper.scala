package li.cil.oc.common.audio.synth.waveshaper

import li.cil.oc.common.audio.synth.channel.Channel
import li.cil.oc.common.audio.synth.parameter.Parameter

import java.nio.FloatBuffer

class Waveshaper(var shaper: ShapingFunction, channel: Option[Channel]) {
  object Drive extends Parameter(0, channel) {
    override def low: Float = 1f / 256

    override def high: Float = 256f

    override def curve: Parameter.Curve = Parameter.Curve.Exponential
  }

  def process(buf: FloatBuffer): Unit = {
    for (i <- buf.position() until buf.limit()) {
      buf.put(shaper.shape(buf.get(i), Drive.value))
      Drive.update()
    }
  }
}
