package li.cil.oc.common.audio.synth

import li.cil.oc.common.audio.synth.channel.{Channel, ChannelId, LfoChannel, MasterChannel, VoiceChannel}
import li.cil.oc.common.audio.synth.oscillator.WavetableOscillator.Wavetable
import li.cil.oc.common.audio.synth.routing.RoutingGraph

import scala.annotation.unused

class Synth(voiceChannelCount: Int, lfoChannelCount: Int) extends Resettable {
  private val voiceChannels = Array.tabulate(voiceChannelCount)(idx => new VoiceChannel(this, ChannelId.Voice(idx)))
  private val lfoChannels = Array.tabulate(lfoChannelCount)(idx => new LfoChannel(this, ChannelId.Lfo(idx)))
  private val masterChannel = new MasterChannel(this)

  private val graph = new RoutingGraph(voiceChannelCount, lfoChannelCount)

  var wavetable: Wavetable = Wavetable.Default

  def channelById(id: ChannelId): Channel = id match {
    case id: ChannelId.Voice => channelById(id)
    case id @ ChannelId.Master => channelById(id)
    case id: ChannelId.Lfo => channelById(id)
  }

  def channelById(id: ChannelId.Voice): VoiceChannel = voiceChannels(id.idx)
  def channelById(@unused id: ChannelId.Master.type): MasterChannel = masterChannel
  def channelById(id: ChannelId.Lfo): LfoChannel = lfoChannels(id.idx)

  @throws[IllegalArgumentException]("if adding causes a send cycle")
  def addChannelSend(sourceId: ChannelId, targetId: ChannelId, target: AnyRef): Unit = {
    if (!graph.add(sourceId, targetId, target)) {
      throw new IllegalArgumentException("channel cycle detected")
    }
  }

  def removeChannelSend(sourceId: ChannelId, targetId: ChannelId, target: AnyRef): Unit = {
    graph.remove(sourceId, targetId, target)
  }

  override def reset(): Unit = {
    super.reset()

    ???
  }

  def process(samples: Int): Unit = {
    for (channelId <- graph.toposort) {
      channelById(channelId).process(samples)
    }

    // TODO: mix channels together and run the master channel's process().
    ???
  }
}
