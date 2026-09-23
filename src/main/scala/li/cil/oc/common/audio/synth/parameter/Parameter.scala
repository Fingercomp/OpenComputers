package li.cil.oc.common.audio.synth.parameter

import li.cil.oc.common.audio.synth.channel.Channel

/**
 * A bounded float value controlled by a source.
 *
 * @param initialSourceValue the initial value to be provided by the default constant modulation source
 * @param channel the channel this parameter is associated with, if any
 */
abstract class Parameter(private val initialSourceValue: Float, val channel: Option[Channel]) {
  /** The minimum value of the parameter. */
  def low: Float

  /** The maximum value of the parameter. */
  def high: Float

  /** The curve that maps the input to [low; high]. */
  def curve: Parameter.Curve = Parameter.Curve.Linear

  private var _source: ModulationSource = ModulationSource.Constant(initialSourceValue)

  final def source: ModulationSource = _source

  final def source_=(source: ModulationSource): Unit = {
    _source.onUnbound(this)
    _source = source
    source.onBound(this)
  }

  final def value: Float = curve.map(source.value, low, high)

  final def update(): Float = {
    val prev = value
    source.update()

    prev
  }

  final def reset(): Unit = {
    source = ModulationSource.Constant(initialSourceValue)
  }
}

object Parameter {
  /** A mapping from [0; 1] to [low; high]. */
  trait Curve {
    /** Scales the input value (taken from [0; 1]) to a value between `low` and `high` according to the curve. */
    def map(value: Float, low: Float, high: Float): Float
  }

  object Curve {
    object Linear extends Curve {
      override def map(value: Float, low: Float, high: Float): Float = value * high + (1 - value) * low
    }

    object Exponential extends Curve {
      override def map(value: Float, low: Float, high: Float): Float = low * math.pow(high / low, value).toFloat
    }
  }
}
