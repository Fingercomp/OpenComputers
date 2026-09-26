package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.common.audio.synth.SignalGenerator
import li.cil.oc.common.audio.synth.channel.Triggerable

abstract class Oscillator(protected var model: OscillatorModel)
  extends SignalGenerator
    with Triggerable
    with OscillatorModel.Listener
