package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.oscillator.WavetableOscillator.Wavetable
import li.cil.oc.common.audio.synth.{SignalGenerator, Synth}

import java.util
import scala.jdk.CollectionConverters.IteratorHasAsScala

/** A synth channel, capable of producing audio signal. */
abstract class Channel(val synth: Synth, val id: ChannelId) extends SignalGenerator {
  private var _open = false

  var outputGain = 1f

  override def reset(): Unit = {
    outputGain = 1f
    super.reset()
  }

  /** Called by [[Synth]] when the wavetable is changed. */
  def onWavetableChanged(wavetable: Wavetable): Unit = {}

  /**
   * Opens the channel.
   *
   * If the channel was not already open, acts as a note on event and triggers all associated envelopes.
   * Otherwise, does nothing.
   */
  final def open(): Unit = {
    if (!_open) {
      _open = true
      onOpen()
    }
  }

  /**
   * Closes the channel.
   *
   * If the channel was not already closed, acts as a note of event and releases all associated envelopes.
   * The channel may still produce sound after that, potentially (with feedback) indefinitely.
   */
  final def close(): Unit = {
    if (_open) {
      _open = false
      onClose()
    }
  }

  /** Returns `true` if the channel is currently open. */
  final def isOpen: Boolean = _open

  /** Called after the channel changes its state to open. */
  protected def onOpen(): Unit = {
    for (listener <- boundListeners.keySet().iterator().asScala) {
      listener.onOpen()
    }
  }

  /** Called after the channel changes its state to closed. */
  protected def onClose(): Unit = {
    for (listener <- boundListeners.keySet().iterator().asScala) {
      listener.onClose()
    }
  }

  private val boundListeners = new util.IdentityHashMap[Triggerable, Unit]

  def bindListener(listener: Triggerable): Unit = {
    boundListeners.put(listener, ())

    // just `.put()` on its own may return `null`, which I'd rather not.
    ()
  }

  def unbindListener(listener: Triggerable): Unit = {
    boundListeners.remove(listener, ())
  }
}
