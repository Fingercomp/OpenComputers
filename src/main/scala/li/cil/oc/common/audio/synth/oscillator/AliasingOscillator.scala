package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.Settings

import java.util.Random

/** A base class for basic waveforms that do not perform antialiasing. Suitable for LFOs. */
abstract class AliasingOscillator extends Oscillator {
  private var phase = 0f
  private var deltaPhase = 0f

  override def reset(): Unit = {
    phase = 0f
    deltaPhase = 0f
  }

  override def setFrequency(freq: Float): Unit = {
    deltaPhase = freq * Settings.get.audioSamplePeriodSec
  }

  override def process(samples: Int): Unit = {
    for (_ <- 0 until samples) {
      output.put(computeSample(phase))

      phase += deltaPhase

      if (phase > 1f) {
        phase -= 1f
        onPhaseReset()
      }
    }
  }

  protected def computeSample(phase: Float): Float

  protected def onPhaseReset(): Unit = {}
}

object AliasingOscillator {
  /** An aliasing sine oscillator. Suitable for LFOs. */
  class Sine extends AliasingOscillator {
    override protected def computeSample(phase: Float): Float = math.sin(2 * Math.PI * phase).toFloat
  }

  /** An aliasing square oscillator. Suitable for LFOs. */
  class Square extends AliasingOscillator {
    override protected def computeSample(phase: Float): Float = {
      if (phase < 0.5f) {
        1f
      } else {
        -1f
      }
    }
  }

  /** An aliasing sawtooth oscillator. Suitable for LFOs. */
  class Sawtooth extends AliasingOscillator {
    override protected def computeSample(phase: Float): Float = 2 * phase - 1
  }

  /** An aliasing triangle oscillator. Suitable for LFOs. */
  class Triangle extends AliasingOscillator {
    override protected def computeSample(phase: Float): Float = {
      if (phase < 0.5f) {
        2 * phase
      } else {
        2 - 2 * phase
      }
    }
  }

  /** An aliasing sample-and-hold noise generator. */
  class SampleAndHoldNoise extends AliasingOscillator {
    private val random = new Random
    private var value = 0f

    override def reset(): Unit = {
      super.reset()
      value = 0f
    }

    override protected def computeSample(phase: Float): Float = value

    override protected def onPhaseReset(): Unit = {
      super.onPhaseReset()
      value = random.nextFloat()
    }
  }

  /** An aliasing LFSR noise generator. */
  class LfsrNoise(private val initial: Int, private val mask: Int) extends AliasingOscillator {
    private var state = initial
    private var value = 1f

    override def reset(): Unit = {
      super.reset()
      state = initial
      value = 1f
    }

    override protected def computeSample(phase: Float): Float = value

    override protected def onPhaseReset(): Unit = {
      super.onPhaseReset()

      val b = state & 1
      state = (state >>> 1) ^ b * mask
      value = b * 2 - 1
    }
  }
}
