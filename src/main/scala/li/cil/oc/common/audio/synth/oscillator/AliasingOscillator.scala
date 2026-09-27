package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.Settings

import java.util.Random

/**
 * A base class for basic waveforms that do not perform antialiasing. Suitable for LFOs.
 *
 * Does not support hard sync.
 */
abstract class AliasingOscillator(model: OscillatorModel) extends Oscillator(model) {
  private var phase = 0f
  private var deltaPhase = 0f

  reset()

  override def reset(): Unit = {
    phase = 0f
    deltaPhase = 0f
    setFrequency(model.frequency)
  }

  private def setFrequency(freq: Float): Unit = {
    deltaPhase = freq * Settings.get.audioSamplePeriodSec
  }

  override def onFrequencyChanged(freq: Float): Unit = {
    setFrequency(freq)
  }

  override def process(samples: Int): Unit = {
    for (_ <- 0 until samples) {
      model.Sync.update()
      output.put(computeSample(phase))

      phase += deltaPhase

      model.fm match {
        case Some(fm) =>
          phase += fm.get() * Settings.get.audioSamplePeriodSec

          // use floor/ceil because of fm.
          if (phase > 1f) {
            phase -= phase.floor
            onPhaseReset()
          } else if (phase < 0f) {
            phase += 1 - phase.ceil
            onPhaseReset()
          }

        case None =>
          if (phase > 1f) {
            phase -= 1f
            onPhaseReset()
          }
      }
    }
  }

  protected def computeSample(phase: Float): Float

  protected def onPhaseReset(): Unit = {}
}

object AliasingOscillator {
  /** An aliasing sine oscillator. Suitable for LFOs. */
  class Sine(model: OscillatorModel) extends AliasingOscillator(model) {
    override protected def computeSample(phase: Float): Float = math.sin(2 * Math.PI * phase).toFloat
  }

  /** An aliasing square oscillator. Suitable for LFOs. */
  class Square(model: OscillatorModel) extends AliasingOscillator(model) {
    override protected def computeSample(phase: Float): Float = {
      if (phase < 0.5f) {
        1f
      } else {
        -1f
      }
    }
  }

  /** An aliasing sawtooth oscillator. Suitable for LFOs. */
  class Sawtooth(model: OscillatorModel) extends AliasingOscillator(model) {
    override protected def computeSample(phase: Float): Float = 2 * phase - 1
  }

  /** An aliasing triangle oscillator. Suitable for LFOs. */
  class Triangle(model: OscillatorModel) extends AliasingOscillator(model) {
    override protected def computeSample(phase: Float): Float = {
      if (phase < 0.5f) {
        2 * phase
      } else {
        2 - 2 * phase
      }
    }
  }

  /** An aliasing sample-and-hold noise generator. */
  class SampleAndHoldNoise(model: OscillatorModel) extends AliasingOscillator(model) {
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
  class LfsrNoise(
    model: OscillatorModel,
    private val initial: Int,
    private val mask: Int,
  ) extends AliasingOscillator(model) {
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
