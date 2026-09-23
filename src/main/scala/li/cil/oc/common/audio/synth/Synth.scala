package li.cil.oc.common.audio.synth

import li.cil.oc.common.audio.synth.channel.VoiceChannel

class Synth(voiceChannelCount: Int, lfoChannelCount: Int) {
  private val voiceChannels = Array.fill(voiceChannelCount)(new VoiceChannel)
}
