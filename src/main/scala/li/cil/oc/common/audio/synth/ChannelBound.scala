package li.cil.oc.common.audio.synth

/** Implemented by classes that are bound to a channel. */
trait ChannelBound {
  def onOpen(): Unit = {}

  def onClose(): Unit = {}
}
