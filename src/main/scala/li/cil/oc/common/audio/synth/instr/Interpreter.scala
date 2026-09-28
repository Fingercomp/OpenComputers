package li.cil.oc.common.audio.synth.instr

import li.cil.oc.common.audio.synth.Synth

class Interpreter(voiceChannelCount: Int, lfoChannelCount: Int) {
  val synth: Synth = new Synth(voiceChannelCount, lfoChannelCount)

  def run(seconds: Float): Unit = ???
}
