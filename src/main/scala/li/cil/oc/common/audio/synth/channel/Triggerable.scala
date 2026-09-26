package li.cil.oc.common.audio.synth.channel

trait Triggerable {
  def onOpen(): Unit = {}

  def onClose(): Unit = {}
}
