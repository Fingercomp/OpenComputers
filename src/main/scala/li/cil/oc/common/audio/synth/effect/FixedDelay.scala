package li.cil.oc.common.audio.synth.effect

import li.cil.oc.common.audio.synth.channel.Channel
import li.cil.oc.common.audio.synth.filter.Filter

import java.nio.FloatBuffer
import java.util

/**
 * A delay element: mixes an input signal with its delayed copy.
 *
 * The delay (specified in samples) is constant and cannot be modulated.
 */
class FixedDelay(delay: Int, channel: Channel) extends Filter {
  require(delay > 0, "delay must be at least 1 sample")

  val Dry = new Filter.Mix(1f, channel)
  val Wet = new Filter.Mix(0f, channel)
  val Feedback = new Filter.Mix(0f, channel)

  // a ring buffer.
  private val buffer = Array.fill(delay)(0f)
  private var idx = 0

  override def reset(): Unit = {
    Dry.reset()
    Wet.reset()
    Feedback.reset()
    util.Arrays.fill(buffer, 0f)
    idx = 0
  }

  override def process(input: FloatBuffer, output: FloatBuffer): Unit = {
    while (input.hasRemaining) {
      val dry = Dry.update()
      val wet = Wet.update()
      val fb = Feedback.update()

      val in = input.get()
      val delayed = buffer(idx)
      val out = dry * in + wet * delayed
      output.put(out)
      buffer(idx) = (1 - fb) * in + fb * out

      idx += 1

      if (idx >= buffer.length) {
        idx = 0
      }
    }
  }
}
