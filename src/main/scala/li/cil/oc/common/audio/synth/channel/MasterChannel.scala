package li.cil.oc.common.audio.synth.channel

import li.cil.oc.Settings
import li.cil.oc.common.audio.synth.Synth

import java.nio.FloatBuffer

class MasterChannel(synth: Synth) extends Channel(synth, ChannelId.Master) with FilteredChannel {
  val input: FloatBuffer = FloatBuffer.allocate(Settings.get.audioBlockSize)

  override def process(samples: Int): Unit = {
    super.process(samples)

    for (_ <- 0 until samples) {
      var out = input.get()

      for (filter <- filter) {
        out = filter.processSample(out)
      }

      output.put(out * outputGain)
    }
  }
}
