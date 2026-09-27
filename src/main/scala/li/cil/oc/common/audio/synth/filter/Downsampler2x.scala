package li.cil.oc.common.audio.synth.filter

import li.cil.oc.common.audio.synth.SampleBuffer
import li.cil.oc.common.audio.synth.filter.Downsampler2x.{Coeffs, Taps}

import java.util

/** A 2× downsampler. Reduces the sample rate of input in half. */
class Downsampler2x extends Filter {
  // the buffer is twice as long as needed so that all input samples are contiguous.
  private val buffer = Array.fill(Taps * 2)(0f)
  private var idx = 0

  /**
    * Processes the input, performing 2× downsampling.
    *
    * Writes the result to the output, which must have enough space for half the remaining samples in the input.
    * The input may contain up to one unprocessed sample after the call.
    */
  def process(input: SampleBuffer, output: SampleBuffer): Unit = {
    while (input.remaining >= 2) {
      buffer(Taps + idx) = input.get()
      buffer(Taps + idx + 1) = input.get()

      // apply a low-pass filter to avoid aliasing high frequencies.
      var out = 0f

      for (i <- idx until idx + Coeffs.length) {
        // the coefficient array is symmetric, so process two at a time.
        out += buffer(i) * Coeffs(i) + buffer(Coeffs.length + i) * Coeffs(Coeffs.length - i - 1)
      }

      output.put(out)

      // decimate by processing every other sample.
      idx += 2

      if (idx >= Taps) {
        // there's no room for new samples in the buffer anymore, so move them back to the start.
        System.arraycopy(buffer, idx, buffer, 0, Taps)
        idx = 0
      }
    }
  }

  /**
    * Resets the internal state of the downsampler.
    *
    * Does not affect the input or output buffers.
    */
  def reset(): Unit = {
    super.reset()
    util.Arrays.fill(buffer, 0f)
    idx = 0
  }
}

object Downsampler2x {
  // coefficients for a low-pass FIR filter (cutoff at f_s / 4).
  private val Coeffs = Array(
    -0.0029803390127006884f,
    -0.0057720116896948365f,
    -0.0013411413481900349f,
    0.011993386389739786f,
    0.021625049053783044f,
    0.013663304501864025f,
    -0.004469214673614284f,
    -0.008735167226704539f,
    0.00675823388485946f,
    0.01620392980439098f,
    0.000029137656644356266f,
    -0.019013495193394823f,
    -0.006886577046117537f,
    0.02208186663576051f,
    0.018204081091408243f,
    -0.021472491144508235f,
    -0.03316510939489422f,
    0.0158340882108002f,
    0.054074969432409525f,
    -0.00015112475178510684f,
    -0.08770032367551749f,
    -0.04297750705433049f,
    0.1831041882793475f,
    0.40896961679050575f,
  )

  private val Taps = Coeffs.length * 2
}
