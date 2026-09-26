package li.cil.oc.common.audio.synth.channel

sealed trait ChannelId

object ChannelId {
  case class Voice(idx: Int) extends ChannelId
  case object Master extends ChannelId
  case class Lfo(idx: Int) extends ChannelId
}
