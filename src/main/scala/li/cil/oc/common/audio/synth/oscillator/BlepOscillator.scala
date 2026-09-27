package li.cil.oc.common.audio.synth.oscillator

import li.cil.oc.Settings

abstract class BlepOscillator(model: OscillatorModel) extends Oscillator(model) {
  // follower phase, in [0; 1).
  protected var phase = 0f

  // sync leader phase, in [0; 1).
  protected var syncPhase = 0f

  // leader phase delta per sample; in [0; 0.5].
  // runs at the fundamental frequency.
  private var deltaSyncPhase = 0f

  // added to the next sample; effectively a 1-sample delay.
  protected var delayedOut = 0f

  setFrequency(model.frequency)

  override def reset(): Unit = {
    syncPhase = 0f
    deltaSyncPhase = 0f
    delayedOut = 0f
    resetFollower()
    setFrequency(model.frequency)
  }

  private def setFrequency(freq: Float): Unit = {
    deltaSyncPhase = freq * Settings.get.audioSamplePeriodSec
  }

  override def onFrequencyChanged(freq: Float): Unit = {
    setFrequency(freq)
  }

  override def process(samples: Int): Unit = {
    for (_ <- 0 until samples) {
      output.put(nextSample())
      model.Sync.update()
    }
  }

  private def nextSample(): Float = {
    // these are the most basic waveforms, so why is this code so complicated? there are two reasons.
    //
    // the first reason is that ideal square, sawtooth, and triangle waves all have infinite harmonics —
    // components of the form sin(2πk * freq + φ), for k ∈ ℕ. however, a digital signal sampled at the rate of
    // sampleFreq times per second can only faithfully represent frequencies up to sampleFreq / 2 (called the Nyquist
    // frequency). any higher-frequency component gets folded back into the spectrum, which is called aliasing.
    //
    // suppose we have a sawtooth oscillator at 10 kHz with the sample rate of 48 kHz. the first two harmonics,
    // at 10 kHz and 20 kHz, get preserved. the next harmonic, at 30 kHz, is above the Nyquist frequency of 24 kHz,
    // so it gets aliased to 2 * 24 kHz - 30 kHz = 18 kHz, followed by 8 kHz, 2 kHz, 12 kHz, etc. these are very much
    // audible and make the result sound nothing like the sawtooth we expect.
    //
    // now, 10 kHz is quite a high frequency, and it's unlikely that the user would actually choose it. that's where the
    // second problem comes in: we have hard sync. it makes the oscillator run at a multiple of the base frequency.
    // so the user could set the frequency to 2093 Hz (corresponds to C7) and the sync to 2.5, which would produce a
    // 5233 Hz sawtooth wave, the 9th harmonic of which would alias back to 907 Hz. the sync itself introduces a bunch
    // of harmonic content into the spectrum, which also alias.
    //
    // all of this is to say, high notes are going to sound horrible. ideally, we should replace the basic waveforms
    // with their bandlimited counterparts (that is to say, only produce as many harmonics as we can fit under the
    // Nyquist frequency). however, then low frequencies, which are used much more often, would require evaluating a
    // crazy number of harmonics (480 for a 50 Hz sawtooth, for example). we can't afford that.
    //
    // BLEPs (and BLAMPs) allow us to reduce aliasing significantly at a much lower cost. the idea is that infinite
    // harmonics arise from discontinuities in the signal. if we replace sudden jumps with something less problematic,
    // we can reduce aliasing greatly. in this case we use polynomial approximations of integrals of windowed sinc.
    // when we encounter a step discontinuity, we replace it with a BLEP (band-limited step). these happen in square and
    // sawtooth waveforms but also when we reset the oscillator due to hard sync. for triangle, we put a BLAMP
    // (band-limited ramp). for convenience, we formulate the BLEP/BLAMP polynomials as corrections to discontinuities.
    // for example, when the square goes from high to low, we do the jump and add a BLEP centered at the discontinuity.
    // we have to delay the output signal by one sample as a result.
    var out = delayedOut
    delayedOut = 0

    val deltaSyncPhase = model.fm match {
      case Some(fm) =>
        // no antialiasing guarantees are made in the presence of fm.
        val delta = this.deltaSyncPhase + fm.get() * Settings.get.audioSamplePeriodSec

        // clamp it within the expected range.
        if (delta > 1f) {
          delta - delta.floor
        } else if (delta < 0f) {
          delta + 1 - delta.ceil
        } else {
          delta
        }

      case None => this.deltaSyncPhase
    }

    if (deltaSyncPhase < 1e-6f) {
      // this is basically DC.
      return computeAliasedSample()
    }

    val deltaPhase = deltaSyncPhase * model.Sync.value
    syncPhase += deltaSyncPhase

    // if the leader finishes a cycle, it triggers an early follower reset.
    val (resetTime, leaderReset) = if (syncPhase >= 1f) {
      syncPhase -= 1f

      (1f - syncPhase / deltaSyncPhase, true)
    } else {
      (1f, false)
    }

    // run the follower until the next reset.
    phase += resetTime * deltaPhase
    out += processFollowerDiscontinuities(deltaPhase)
    val aliasedValue = computeAliasedSample()
    delayedOut += aliasedValue

    if (leaderReset) {
      // the follower was reset by the leader: process the resulting discontinuity.
      val t = 1 - phase / deltaPhase
      resetFollower()

      val resetValue = computeAliasedSample()
      val stepSize = resetValue - aliasedValue
      out += stepSize * BlepOscillator.blep0(t)
      delayedOut += stepSize * BlepOscillator.blep1(t)

      // now process the rest of the sample.
      phase = (1 - resetTime) * deltaPhase
      out += processFollowerDiscontinuities(deltaPhase)
      delayedOut += computeAliasedSample()
    }

    out * 2 - 1
  }

  protected def processFollowerDiscontinuities(deltaPhase: Float): Float

  protected def computeAliasedSample(): Float

  protected def resetFollower(): Unit = {
    phase = 0f
  }
}

object BlepOscillator {
  def blep0(t: Float): Float = 0.5f * t * t
  def blep1(t: Float): Float = -blep0(1 - t)

  private val sixth = 1f / 6

  def blamp0(t: Float): Float = t * t * t * sixth
  def blamp1(t: Float): Float = -blamp0(1 - t)

  class Square(model: OscillatorModel) extends BlepOscillator(model) {
    // 0 -> high, 1 -> low
    private var stage: Byte = 0

    override protected def resetFollower(): Unit = {
      super.resetFollower()
      stage = 0
    }

    override protected def processFollowerDiscontinuities(deltaPhase: Float): Float = {
      var out = 0f

      // we need a loop because the follower's frequency can reach 20 kHz * 3 = 60 kHz (3 being the hard sync rate),
      // so we may run several cycles in one sample.
      while (true) {
        if (stage == 0) {
          if (phase < 0.5f) {
            return out
          }

          val t = 1 - (phase - 0.5f) / deltaPhase
          out += BlepOscillator.blep0(t)
          delayedOut += BlepOscillator.blep1(t)
          stage = 1
        }

        if (stage == 1) {
          if (phase < 1f) {
            return out
          }

          val t = 1 - (phase - 1f) / deltaPhase
          out -= BlepOscillator.blep0(t)
          delayedOut -= BlepOscillator.blep1(t)
          phase -= 1f
          stage = 0
        }
      }

      out
    }

    override protected def computeAliasedSample(): Float = 1f - stage.toFloat
  }

  class Sawtooth(model: OscillatorModel) extends BlepOscillator(model) {
    override def processFollowerDiscontinuities(deltaPhase: Float): Float = {
      var out = 0f

      while (phase >= 1f) {
        val t = 1 - (phase - 1f) / deltaPhase
        out -= BlepOscillator.blep0(t)
        delayedOut -= BlepOscillator.blep1(t)
        phase -= 1f
      }

      out
    }

    override protected def computeAliasedSample(): Float = phase
  }

  class Triangle(model: OscillatorModel) extends BlepOscillator(model) {
    private var stage: Byte = 0

    override protected def resetFollower(): Unit = {
      super.resetFollower()
      stage = 0
    }

    override def processFollowerDiscontinuities(deltaPhase: Float): Float = {
      var out = 0f

      while (true) {
        if (stage == 0) {
          if (phase < 0.5f) {
            return out
          }

          val t = 1 - (phase - 0.5f) / deltaPhase
          out += BlepOscillator.blamp0(t)
          delayedOut += BlepOscillator.blamp1(t)
          stage = 1
        }

        if (stage == 1) {
          if (phase < 1f) {
            return out
          }

          val t = 1 - (phase - 1f) / deltaPhase
          out -= BlepOscillator.blamp0(t)
          delayedOut -= BlepOscillator.blamp1(t)
          phase -= 1f
          stage = 0
        }
      }

      out
    }

    override protected def computeAliasedSample(): Float = {
      if (stage == 0) {
        phase * 2
      } else {
        (1 - phase) * 2
      }
    }
  }
}
