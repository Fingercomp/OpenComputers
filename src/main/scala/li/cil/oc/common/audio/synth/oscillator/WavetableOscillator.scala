package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.Settings
import li.cil.oc.common.audio.synth.AudioBuffer
import li.cil.oc.common.audio.synth.filter.Downsampler2x
import li.cil.oc.common.audio.synth.parameter.Parameter

import java.nio.FloatBuffer

/** An oscillator that samples a wavetable to produce samples. */
class WavetableOscillator(model: OscillatorModel) extends Oscillator(model) {
  object Morph extends Parameter(0f, Some(model.channel)) {
    override def low: Float = 0f

    override def high: Float = 1f
  }

  // normalized position within a wavetable entry, in [0; 1).
  private var phase = 0f

  // phase increment per sample, in [0; 0.5).
  private var deltaPhase = 0f

  // perform oversampling to reduce aliasing somewhat.
  private val oversampledOutput = new AudioBuffer(Settings.get.audioBlockSize * 2)
  private val downsampler = new Downsampler2x

  private var wavetable = model.wavetable

  setFrequency(model.frequency)

  override def reset(): Unit = {
    Morph.reset()
    phase = 0f
    deltaPhase = 0f
    downsampler.reset()
    setFrequency(model.frequency)
    wavetable = model.wavetable
  }

  private def setFrequency(freq: Float): Unit = {
    // the formula is freq / sampleRate; divide by 2 since the sample rate we're working with is two times higher.
    deltaPhase = freq * Settings.get.audioSamplePeriodSec * 0.5f
  }

  override def onFrequencyChanged(freq: Float): Unit = {
    setFrequency(freq)
  }

  override def onWavetableChanged(wavetable: WavetableOscillator.Wavetable): Unit = {
    this.wavetable = wavetable
  }

  override def process(samples: Int): Unit = {
    for (_ <- 0 until samples) {
      oversampledOutput.put(nextSample())
      oversampledOutput.put(nextSample())
      Morph.update()
    }

    downsampler.process(oversampledOutput, output)
  }

  private def nextSample(): Float = {
    val out = wavetable.sample(Morph.value, phase)
    phase += deltaPhase

    if (phase >= 1f) {
      phase -= 1f
    }

    out
  }
}

object WavetableOscillator {
  class Wavetable(input: FloatBuffer, val waveSize: Int, val oneshot: Boolean) {
    val tableSize: Int = input.limit() / waveSize

    if (tableSize <= 0 || tableSize >= Settings.get.audioCardMaxWavetableSize) {
      throw new IllegalArgumentException(
        s"wave size $waveSize resulted in illegal wavetable size $tableSize (must be between 1 and ${Settings.get.audioCardMaxWavetableSize})"
      )
    }

    private val samples = FloatBuffer.allocate(tableSize * (waveSize + 1))

    {
      var pos = 0

      for (_ <- 0 until tableSize) {
        samples.put(input.slice(pos, tableSize))

        // for easier sampling add one more sample.
        if (oneshot) {
          samples.put(input.get(pos + tableSize - 1))
        } else {
          samples.put(input.get(pos))
        }

        pos += tableSize
      }
    }

    private def sample(waveIdx: Int, sampleIdx: Int): Float = {
      samples.get(waveIdx * (waveSize + 1) + sampleIdx)
    }

    def sample(morph: Float, phase: Float): Float = {
      val wavePos = morph * (tableSize - 1)
      val waveIdx = wavePos.toInt
      val mix = wavePos - waveIdx.toFloat

      val samplePos = phase * waveSize
      val sampleIdx = samplePos.toInt
      val t = samplePos - sampleIdx.toFloat

      // perform linear interpolation between adjacent samples.
      val a = (1 - t) * sample(waveIdx, sampleIdx) + t * sample(waveIdx, sampleIdx + 1)
      val b = (1 - t) * sample(waveIdx + 1, sampleIdx) + t * sample(waveIdx + 1, sampleIdx + 1)

      // mix the two adjacent waves linearly.
      (1 - mix) * a + mix * b
    }
  }

  object Wavetable {
    val Default: Wavetable = generateDefault()

    private def generateDefault(): Wavetable = {
      val waveSize = 2048
      val tableSize = 4
      val input = FloatBuffer.allocate(waveSize * tableSize)

      for (waveIdx <- 0 until tableSize) {
        var phase = 0f
        val deltaPhase = (1 + waveIdx).toFloat / waveSize

        for (_ <- 0 until waveSize) {
          input.put(phase)
          phase += deltaPhase

          if (phase >= 1f) {
            phase -= 1f
          }
        }
      }

      new Wavetable(input, waveSize, oneshot = false)
    }
  }
}
