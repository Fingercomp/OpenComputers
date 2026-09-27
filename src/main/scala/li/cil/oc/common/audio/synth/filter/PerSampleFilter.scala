package li.cil.oc.common.audio.synth.filter

import li.cil.oc.common.audio.synth.SampleBuffer

trait PerSampleFilter extends Filter {
  def processSample(input: Float): Float

  override def process(input: SampleBuffer, output: SampleBuffer): Unit = {
    while (input.nonEmpty) {
      output.put(processSample(input.get()))
    }
  }
}
