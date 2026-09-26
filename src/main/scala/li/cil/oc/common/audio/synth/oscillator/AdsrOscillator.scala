package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.Settings
import li.cil.oc.common.audio.synth.oscillator.AdsrOscillator.State
import li.cil.oc.common.audio.synth.parameter.Parameter

class AdsrOscillator(model: OscillatorModel) extends Oscillator(model) {
  private class TimeParameter extends Parameter(0, Some(model.channel)) {
    override def low: Float = 0

    override def high: Float = 60000
  }

  val Attack, Decay, Release = new TimeParameter

  object Sustain extends Parameter(1, Some(model.channel)) {
    override def low: Float = 0f

    override def high: Float = 1f
  }

  private var state: State = State.Closed
  private var phase = 0f
  private var valueAtClose = 0f

  override def reset(): Unit = {
    Attack.reset()
    Decay.reset()
    Release.reset()
    Sustain.reset()
    setState(State.Closed)
  }

  private def setState(state: State): Unit = {
    this.state = state
    phase = 0f
  }

  override def onOpen(): Unit = {
    super.onOpen()
    setState(State.Attack)
  }

  override def onClose(): Unit = {
    super.onClose()
    valueAtClose = computeValue(state)
    setState(State.Release)
  }

  override def onFrequencyChanged(freq: Float): Unit = {
    // ignore.
  }

  override def process(samples: Int): Unit = {
    for (_ <- 0 until samples) {
      output.put(nextSample())
    }
  }

  private def computeValue(state: State): Float = state match {
    case State.Attack => phase
    case State.Decay => 1f - phase * (1f - Sustain.value)
    case State.Sustain => Sustain.value
    case State.Release => valueAtClose * (1f - phase)
    case State.Closed => 0f
  }

  private def processState(timeSec: Float): Boolean = {
    if (timeSec < Settings.get.audioSamplePeriodSec * 0.5f) {
      phase = 1f
    }

    if (phase >= 1f) {
      setState(state.next)

      false
    } else {
      phase += timeSec * Settings.get.audioSamplePeriodSec

      true
    }
  }

  private def nextSample(): Float = {
    if (state == State.Attack && processState(Attack.value * 1e-3f)) {
      return computeValue(State.Attack)
    }

    if (state == State.Decay && processState(Decay.value * 1e-3f)) {
      return computeValue(State.Decay)
    }

    if (state == State.Sustain) {
      return computeValue(State.Sustain)
    }

    if (state == State.Release && processState(Release.value * 1e-3f)) {
      return computeValue(State.Release)
    }

    computeValue(State.Closed)
  }
}

object AdsrOscillator {
  private sealed trait State {
    def next: State
  }

  private object State {
    case object Attack extends State {
      override def next: State = Decay
    }

    case object Decay extends State {
      override def next: State = Sustain
    }

    case object Sustain extends State {
      override def next: State = Release
    }

    case object Release extends State {
      override def next: State = Closed
    }

    case object Closed extends State {
      override def next: State = Closed
    }
  }
}
