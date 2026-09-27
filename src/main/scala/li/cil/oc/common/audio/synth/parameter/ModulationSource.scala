package li.cil.oc.common.audio.synth.parameter

import li.cil.oc.common.audio.synth
import li.cil.oc.common.audio.synth.channel.Triggerable
import li.cil.oc.common.audio.synth.envelope.AdsrEnvelope

/** Provides a value between 0 and 1. */
sealed trait ModulationSource {
  /** Returns the current value. */
  def value: Float

  /** Advances the source by one sample, returning the previous one. */
  def update(): Float

  /** The parameter this source is currently bound to. */
  protected var parameter: Option[Parameter] = None

  /** Called when this modulation source is bound to a parameter. */
  def onBound(parameter: Parameter): Unit = {
    this.parameter = Some(parameter)
  }

  /** Called when this modulation source is unbound from a parameter. */
  def onUnbound(parameter: Parameter): Unit = {
    this.parameter = None
  }
}

object ModulationSource {
  case class Constant(value: Float) extends ModulationSource {
    override def update(): Float = value
  }

  class Adsr(
    attack: Float,
    decay: Float,
    sustain: Float,
    release: Float,
    low: Float,
    high: Float,
  ) extends ModulationSource with Triggerable {
    private val envelope = new AdsrEnvelope(None)
    envelope.Attack.source = Constant(attack)
    envelope.Decay.source = Constant(decay)
    envelope.Sustain.source = Constant(sustain)
    envelope.Release.source = Constant(release)

    override def onOpen(): Unit = {
      super.onOpen()
      envelope.onOpen()
    }

    override def onClose(): Unit = {
      super.onClose()
      envelope.onClose()
    }

    private var _value = mapEnvelopeValue(envelope.update())

    private def mapEnvelopeValue(t: Float): Float = {
      (1 - t) * low + t * high
    }

    override def value: Float = _value

    override def update(): Float = {
      val result = _value
      _value = mapEnvelopeValue(envelope.update())

      result
    }

    override def onBound(parameter: Parameter): Unit = {
      super.onBound(parameter)

      for (channel <- parameter.channel) {
        channel.bindListener(this)
      }
    }

    override def onUnbound(parameter: Parameter): Unit = {
      for (channel <- parameter.channel) {
        channel.unbindListener(this)
      }

      super.onUnbound(parameter)
    }
  }

  class Channel(modChannel: synth.channel.Channel) extends ModulationSource {
    private var pos = modChannel.output.rdIdx

    override def value: Float = modChannel.output.getAbsolute(pos)

    override def update(): Float = {
      val prev = value
      pos = modChannel.output.wrapIndex(pos + 1)

      prev
    }

    override def onBound(parameter: Parameter): Unit = {
      super.onBound(parameter)

      for (targetChannel <- parameter.channel) {
        modChannel.synth.addChannelSend(modChannel.id, targetChannel.id, this)
      }
    }

    override def onUnbound(parameter: Parameter): Unit = {
      for (targetChannel <- parameter.channel) {
        modChannel.synth.removeChannelSend(modChannel.id, targetChannel.id, this)
      }

      super.onUnbound(parameter)
    }
  }
}
