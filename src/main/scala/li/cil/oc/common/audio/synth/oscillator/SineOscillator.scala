package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.Settings
import li.cil.oc.common.audio.synth.filter.Downsampler2x

import java.nio.FloatBuffer

class SineOscillator(sync: Oscillator.Sync) extends Oscillator {
  // follower phase, in [0; 1).
  private var phase = 0f

  // sync leader phase, in [0; 1).
  private var syncPhase = 0f

  // sync leader phase increment per sample, in [0; 0.5).
  private var deltaSyncPhase = 0f

  // on its own sine has no aliasing issues, but hard sync makes things complicated. unlike, say, square or triangle, we
  // can't make the problem go away with integration tricks, and so we have to resort to oversampling.
  private val oversampledOutput = FloatBuffer.allocate(Settings.get.audioBlockSize * 2)
  private val downsampler = new Downsampler2x

  override def reset(): Unit = {
    phase = 0f
    syncPhase = 0f
    deltaSyncPhase = 0f
    downsampler.reset()
  }

  override def setFrequency(freq: Float): Unit = {
    // the formula is freq / sampleRate; divide by 2 since the sample rate we're working with is two times higher.
    deltaSyncPhase = freq * Settings.get.audioSamplePeriodSec * 0.5f
  }

  override def process(samples: Int): Unit = {
    oversampledOutput.clear()

    for (i <- 0 until samples * 2) {
      oversampledOutput.put(nextSample())

      if (i % 2 == 0) {
        sync.update()
      }
    }

    downsampler.process(oversampledOutput, output)
  }

  private def nextSample(): Float = {
    syncPhase += deltaSyncPhase

    if (syncPhase >= 1f) {
      syncPhase -= 1f
      phase = syncPhase * sync.value
    } else {
      phase += deltaSyncPhase * sync.value
    }

    // TODO: replace with linear interpolation of tabulated sine.
    math.sin(2 * Math.PI * phase).toFloat
  }
}
