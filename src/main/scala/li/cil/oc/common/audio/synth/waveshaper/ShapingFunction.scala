package li.cil.oc.common.audio.synth.waveshaper

/** Transforms a single sample. */
@FunctionalInterface
trait ShapingFunction {
  def shape(sample: Float, drive: Float): Float
}

object ShapingFunction {
  /** An identity shaper. */
  object Identity extends ShapingFunction {
    override def shape(sample: Float, drive: Float): Float = sample
  }

  /** A soft clipper (hyperbolic tangent). */
  object SoftClip extends ShapingFunction {
    // TODO: tabulate.
    override def shape(sample: Float, drive: Float): Float = math.tanh(sample * drive).toFloat
  }

  /** A hard clipper (clamps between -1 and 1). */
  object HardClip extends ShapingFunction {
    override def shape(sample: Float, drive: Float): Float = sample * drive max -1f min 1f
  }
}
