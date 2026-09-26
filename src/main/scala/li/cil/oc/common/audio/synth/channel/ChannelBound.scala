package li.cil.oc.common.audio.synth.channel

/** Implemented by classes that are bound to a channel. */
trait ChannelBound extends Triggerable {
  /** The channel this object is bound to. */
  def boundChannel: Option[Channel]
}
