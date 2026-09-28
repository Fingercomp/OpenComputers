package li.cil.oc.common.audio.synth.filter

import li.cil.oc.common.audio.synth.channel.Channel

abstract class ChannelFilter(initialCutoff: Float, channel: Channel) extends PerSampleFilter {
  val Cutoff = new Filter.Cutoff(initialCutoff, channel)
  val Resonance = new Filter.Resonance(channel)
}
