package li.cil.oc.common.audio.synth.filter

import li.cil.oc.common.audio.synth.AudioBuffer

trait PerSampleFilter extends Filter {
  def processSample(input: Float): Float

  override def process(input: AudioBuffer, output: AudioBuffer): Unit = {
    while (input.nonEmpty) {
      output.put(processSample(input.get()))
    }
  }
}
