package li.cil.oc.common.audio.synth

import li.cil.oc.common.audio.synth.channel.{Channel, ChannelId, LfoChannel, MasterChannel, OscillatorChannel, VoiceChannel}
import li.cil.oc.common.audio.synth.oscillator.WavetableOscillator.Wavetable
import li.cil.oc.common.audio.synth.routing.RoutingGraph

import java.nio.ByteBuffer
import scala.annotation.unused

class Synth(voiceChannelCount: Int, lfoChannelCount: Int) extends Resettable {
  private val voiceChannels = Array.tabulate(voiceChannelCount)(idx => new VoiceChannel(this, ChannelId.Voice(idx)))
  private val lfoChannels = Array.tabulate(lfoChannelCount)(idx => new LfoChannel(this, ChannelId.Lfo(idx)))
  private val masterChannel = new MasterChannel(this)

  def oscillatorChannels: Iterator[OscillatorChannel] = voiceChannels.iterator ++ lfoChannels.iterator
  def channels: Iterator[Channel] = oscillatorChannels ++ Iterator(masterChannel)

  private val graph = new RoutingGraph(voiceChannelCount, lfoChannelCount)

  private var _wavetable: Wavetable = Wavetable.Default

  def wavetable: Wavetable = _wavetable

  def wavetable_=(wavetable: Wavetable): Unit = {
    if (_wavetable ne wavetable) {
      _wavetable = wavetable

      for (channel <- channels) {
        channel.onWavetableChanged(wavetable)
      }
    }
  }

  def channelById(id: ChannelId): Option[Channel] = id match {
    case id: ChannelId.Voice => channelById(id)
    case id @ ChannelId.Master => Some(channelById(id))
    case id: ChannelId.Lfo => channelById(id)
  }

  def channelById(id: ChannelId.Voice): Option[VoiceChannel] = voiceChannels.lift(id.idx)
  def channelById(@unused id: ChannelId.Master.type): MasterChannel = masterChannel
  def channelById(id: ChannelId.Lfo): Option[LfoChannel] = lfoChannels.lift(id.idx)

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

    for (channel <- channels) {
      channel.reset()
    }

    graph.reset()
    wavetable = Wavetable.Default
  }

  def process(samples: Int, output: ByteBuffer): Unit = {
    for (channelId <- graph.toposort; channel <- channelById(channelId)) {
      channel.process(samples)
    }

    val wrIdx = masterChannel.output.wrIdx

    // skip channels that act as modulators.
    for (voiceChannel <- voiceChannels if !graph.hasSends(voiceChannel.id)) {
      for (i <- 0 until voiceChannel.output.remaining) {
        val out = voiceChannel.output.getAt(i) * voiceChannel.MasterSendLevel.update()
        masterChannel.output.putAbsolute(wrIdx + i, masterChannel.output.getAbsolute(wrIdx + i) + out)
      }
    }

    masterChannel.process(samples)

    for (_ <- 0 until samples) {
      output.put(quantize(masterChannel.output.get()))
    }

    // now update the output buffers so that the next process() has room for writing.
    for (voiceChannel <- channels) {
      voiceChannel.output.skipUnread()
    }
  }

  private def quantize(sample: Float): Byte = {
    val unipolar = (sample + 1) * 0.5f

    (unipolar * 255).toByte
  }
}
