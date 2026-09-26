package li.cil.oc.common.audio.synth

trait SignalGenerator extends Resettable {
  final val output: AudioBuffer = AudioBuffer.makeBlockSized()

  /**
   * Synthesizes the given number of samples in the [[output]].
   *
   * Each sample is represented as a single float (i.e., mono f32 pcm).
   *
   * The number of samples must not exceed `output.remaining()`.
   */
  def process(samples: Int): Unit
}
