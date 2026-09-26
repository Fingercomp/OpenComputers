package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.Synth

class MasterChannel(synth: Synth) extends Channel(synth, ChannelId.Master) with FilteredChannel {
  override def process(samples: Int): Unit = {
    super.process(samples)

    for (_ <- 0 until samples) {
      var out = output.getAbsolute(output.wrIdx)

      for (filter <- filter) {
        out = filter.processSample(out)
      }

      out *= outputGain

      // get rid of NaNs.
      if (out != out) {
        out = 0f
      }

      // clamp the output.
      out = out min 1f max -1f

      output.put(out)
    }
  }
}
