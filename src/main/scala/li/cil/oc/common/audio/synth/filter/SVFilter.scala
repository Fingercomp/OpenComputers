package li.cil.oc.common.audio.synth.filter

import li.cil.oc.Settings
import li.cil.oc.common.audio.synth.channel.Channel

import java.nio.FloatBuffer

/** A base class of low-, band-, and high-pass filters. */
abstract class SVFilter(initialCutoff: Float, channel: Channel) extends Filter {
  val Cutoff = new Filter.Cutoff(initialCutoff, channel)
  val Resonance = new Filter.Resonance(channel)

  private var prevCutoff = -1f
  private var prevReso = -1f

  // filter coefficients.
  protected var g: Float = 0f
  protected var d: Float = 0f

  override def reset(): Unit = {
    super.reset()
    Cutoff.reset()
    Resonance.reset()
  }

  override def process(input: FloatBuffer, output: FloatBuffer): Unit = {
    while (input.hasRemaining) {
      val cutoff = Cutoff.update()
      val reso = Resonance.update()

      if (cutoff != prevCutoff || reso != prevReso) {
        updateCoeffs(cutoff, reso)
        prevCutoff = cutoff
        prevReso = reso
      }

      output.put(processSample(input.get(), cutoff, reso))
    }
  }

  private def updateCoeffs(cutoff: Float, reso: Float): Unit = {
    g = math.tan(Math.PI * cutoff).toFloat * Settings.get.audioSamplePeriodSec
    d = 1f / (1 + 2 * reso * g + g * g)
  }

  protected def processSample(sample: Float, cutoff: Float, reso: Float): Float
}

object SVFilter {
  trait TwoPole extends SVFilter {
    // integrator accumulators.
    protected var s1 = 0f
    protected var s2 = 0f

    override def reset(): Unit = {
      super.reset()
      s1 = 0f
      s2 = 0f
    }
  }

  class LowPass12dB(channel: Channel) extends SVFilter(1f, channel) with TwoPole {
    override protected def processSample(sample: Float, cutoff: Float, reso: Float): Float = {
      val bp = d * (g * (sample - s2) + s1)
      val v1 = bp - s1
      s1 = bp + v1
      val v2 = g * bp
      val lp = v2 + s2
      s2 = lp + v2

      lp
    }
  }

  class BandPass12dB(channel: Channel) extends SVFilter(0.5f, channel) with TwoPole {
    override protected def processSample(sample: Float, cutoff: Float, reso: Float): Float = {
      val bp = d * (g * (2 * reso * sample - s2) + s1)
      val bpDoubled = bp + bp
      s1 = bpDoubled - s1
      s2 += g * bpDoubled

      bp
    }
  }

  class HighPass12dB(channel: Channel) extends SVFilter(0f, channel) with TwoPole {
    override protected def processSample(sample: Float, cutoff: Float, reso: Float): Float = {
      val hp = d * (sample - (2 * reso + g) * s1 - s2)
      val v1 = g * hp
      val bp = v1 + s1
      s1 = bp + v1
      s2 += 2 * g * bp

      hp
    }
  }

  trait FourPole extends SVFilter {
    // integrator accumulators.
    protected var s1 = 0f
    protected var s2 = 0f
    protected var s3 = 0f
    protected var s4 = 0f

    override def reset(): Unit = {
      super.reset()
      s1 = 0f
      s2 = 0f
      s3 = 0f
      s4 = 0f
    }
  }

  class LowPass24dB(channel: Channel) extends SVFilter(1f, channel) with FourPole {
    override protected def processSample(sample: Float, cutoff: Float, reso: Float): Float = {
      val bp1 = d * (g * (sample - s2) + s1)
      val v1 = bp1 - s1
      s1 = bp1 + v1
      val v2 = g * bp1
      val lp1 = v2 + s2
      s2 = lp1 + v2

      val bp2 = d * (g * (lp1 - s4) + s3)
      val v3 = bp2 - s3
      s3 = bp2 + v3
      val v4 = g * bp2
      val lp2 = v4 + s4
      s4 = lp2 + v4

      lp2
    }
  }

  class BandPass24dB(channel: Channel) extends SVFilter(0.5f, channel) with FourPole {
    override protected def processSample(sample: Float, cutoff: Float, reso: Float): Float = {
      val bp1 = d * (g * (2 * reso * sample - s2) + s1)
      val bpDoubled1 = bp1 + bp1
      s1 = bpDoubled1 - s1
      s2 += g * bpDoubled1

      val bp2 = d * (g * (2 * reso * bp1 - s4) + s3)
      val bpDoubled2 = bp2 + bp2
      s3 = bpDoubled2 - s3
      s4 += g * bpDoubled2

      bp2
    }
  }

  class HighPass24dB(channel: Channel) extends SVFilter(0f, channel) with FourPole {
    override protected def processSample(sample: Float, cutoff: Float, reso: Float): Float = {
      val hp1 = d * (sample - (2 * reso + g) * s1 - s2)
      val v1 = g * hp1
      val bp1 = v1 + s1
      s1 = bp1 + v1
      s2 += 2 * g * bp1

      val hp2 = d * (hp1 - (2 * reso + g) * s3 - s4)
      val v3 = g * hp2
      val bp2 = v3 + s3
      s3 = bp2 + v3
      s4 += 2 * g * bp2

      hp2
    }
  }
}
