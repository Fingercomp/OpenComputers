package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.Settings
import li.cil.oc.common.audio.synth.SampleBuffer
import li.cil.oc.common.audio.synth.filter.Downsampler2x

class SineOscillator(model: OscillatorModel, unipolar: Boolean) extends Oscillator(model) {
  // follower phase, in [0; 1).
  private var phase = 0f

  // sync leader phase, in [0; 1).
  private var syncPhase = 0f

  // sync leader phase increment per sample, in [0; 0.5).
  private var deltaSyncPhase = 0f

  // on its own sine has no aliasing issues, but hard sync makes things complicated. unlike, say, square or triangle, we
  // can't make the problem go away with integration tricks, and so we have to resort to oversampling.
  private val oversampledOutput = new SampleBuffer(Settings.get.audioBlockSize * 2)
  private val downsampler = new Downsampler2x

  setFrequency(model.frequency)

  override def reset(): Unit = {
    phase = 0f
    syncPhase = 0f
    deltaSyncPhase = 0f
    downsampler.reset()
    setFrequency(model.frequency)
  }

  private def setFrequency(freq: Float): Unit = {
    // the formula is freq / sampleRate; divide by 2 since the sample rate we're working with is two times higher.
    deltaSyncPhase = freq * Settings.get.audioSamplePeriodSec * 0.5f
  }

  override def onFrequencyChanged(freq: Float): Unit = {
    setFrequency(freq)
  }

  override def process(samples: Int): Unit = {
    for (_ <- 0 until samples) {
      val deltaSyncPhase = model.fm match {
        case Some(fm) =>
          val delta = this.deltaSyncPhase + fm.get() * Settings.get.audioSamplePeriodSec * 0.5f

          if (delta > 1f) {
            delta - delta.floor
          } else if (delta < 0f) {
            delta + 1f - delta.ceil
          } else {
            delta
          }

        case None => this.deltaSyncPhase
      }

      oversampledOutput.put(nextSample(deltaSyncPhase))
      oversampledOutput.put(nextSample(deltaSyncPhase))
      model.Sync.update()
    }

    downsampler.process(oversampledOutput, output)
  }

  private def nextSample(deltaSyncPhase: Float): Float = {
    syncPhase += deltaSyncPhase

    if (syncPhase >= 1f) {
      syncPhase -= 1f
      phase = syncPhase * model.Sync.value
    } else {
      phase += deltaSyncPhase * model.Sync.value
    }

    // TODO: replace with linear interpolation of tabulated sine.
    val out = math.sin(2 * Math.PI * phase).toFloat

    if (unipolar) {
      (1 + out) * 0.5f
    } else {
      out
    }
  }
}
