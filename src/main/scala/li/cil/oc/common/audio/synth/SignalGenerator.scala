package li.cil.oc.common.audio.synth

import li.cil.oc.Settings

import java.nio.FloatBuffer

trait SignalGenerator {
  final val output: FloatBuffer = FloatBuffer.allocate(Settings.get.audioBlockSize)

  /**
   * Synthesizes the given number of samples in the [[output]].
   *
   * Each sample is represented as a single float (i.e., mono f32 pcm).
   *
   * The number of samples must not exceed `output.remaining()`.
   */
  def process(samples: Int): Unit

  /** Resets the channel to its initial state. */
  def reset(): Unit
}
