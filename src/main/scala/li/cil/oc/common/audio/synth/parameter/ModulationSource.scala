package li.cil.oc.common.audio.synth.parameter

import li.cil.oc.Settings
import li.cil.oc.common.audio.synth
import li.cil.oc.common.audio.synth.ChannelBound

/** Provides a value between 0 and 1. */
sealed trait ModulationSource {
  /** Returns the current value. */
  def value: Float

  /** Advances the source by one sample, returning the previous one. */
  def update(): Float

  /** Called when this modulation source is bound to a parameter. */
  def onBound(parameter: Parameter): Unit = {}

  /** Called when this modulation source is unbound from a parameter. */
  def onUnbound(parameter: Parameter): Unit = {}
}

object ModulationSource {
  case class Constant(value: Float) extends ModulationSource {
    override def update(): Float = value
  }

  class Adsr(
    attackMs: Float,
    decayMs: Float,
    sustain: Float,
    releaseMs: Float,
    low: Float,
    high: Float,
  ) extends ModulationSource with ChannelBound {
    private var state: Adsr.State = Adsr.State.Closed
    private var elapsedMs = 0f
    private var _value = 0f

    private val attackRate = 1 / attackMs

    private val decayRate = {
      if (1 - sustain < 1e-4f) {
        0
      } else {
        (1 - sustain) / decayMs
      }
    }

    private val releaseRate = 1 / releaseMs
    private var valueAtClose = 0f

    override def value: Float = _value * high + (1 - _value) * low

    override def onOpen(): Unit = {
      super.onOpen()
      state = Adsr.State.Attack
      elapsedMs = 0
      updateState()
    }

    override def onClose(): Unit = {
      super.onClose()
      state = Adsr.State.Release
      elapsedMs = 0
      valueAtClose = _value
      updateState()
    }

    override def update(): Float = {
      val prev = value
      elapsedMs += Settings.get.audioSamplePeriodMs
      updateState()

      prev
    }

    private def updateState(): Unit = {
      if (state == Adsr.State.Attack) {
        if (elapsedMs >= attackMs) {
          elapsedMs -= attackMs
          _value = 1f
          state = Adsr.State.Decay
        } else {
          _value = (attackRate * elapsedMs).min(1)

          return
        }
      }

      if (state == Adsr.State.Decay) {
        if (elapsedMs >= decayMs) {
          elapsedMs -= decayMs
          _value = sustain
          state = Adsr.State.Sustain
        } else {
          _value = (1 - decayRate * elapsedMs).max(0).min(1)

          return
        }
      }

      if (state == Adsr.State.Release) {
        if (elapsedMs > releaseMs) {
          elapsedMs = 0
          _value = 0
          state = Adsr.State.Closed
        } else {
          _value = (valueAtClose * (1 - releaseRate * elapsedMs)).max(0).min(1)
        }
      }
    }

    override def onBound(parameter: Parameter): Unit = {
      super.onBound(parameter)

      for (channel <- parameter.channel) {
        channel.bindListener(this)
      }
    }

    override def onUnbound(parameter: Parameter): Unit = {
      super.onUnbound(parameter)

      for (channel <- parameter.channel) {
        channel.unbindListener(this)
      }
    }
  }

  private object Adsr {
    sealed trait State

    object State {
      case object Attack extends State
      case object Decay extends State
      case object Sustain extends State
      case object Release extends State
      case object Closed extends State
    }
  }

  class Channel(modChannel: synth.channel.Channel) extends ModulationSource {
    private var pos = modChannel.output.position()

    override def value: Float = modChannel.output.get(pos)

    override def update(): Float = {
      val prev = value
      pos += 1

      if (pos > modChannel.output.limit()) {
        pos = 0
      }

      prev
    }

    override def onBound(parameter: Parameter): Unit = {
      super.onBound(parameter)

      // TODO
      ???
    }

    override def onUnbound(parameter: Parameter): Unit = {
      super.onUnbound(parameter)

      // TODO
      ???
    }
  }
}
