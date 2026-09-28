//noinspection ScalaWeakerAccess
package li.cil.oc.common.audio.synth.instr

import com.mojang.serialization.codecs.RecordCodecBuilder
import com.mojang.serialization.{Codec, MapCodec}
import io.netty.buffer.ByteBuf
import li.cil.oc.common.audio.synth.oscillator._
import li.cil.oc.common.datacomponents.{ScalaCodec, ScalaStreamCodec}
import net.minecraft.network.codec.StreamCodec

sealed trait OscillatorFactory {
  def kind: OscillatorFactory.Kind.Value

  def make(interp: Interpreter, model: OscillatorModel): Oscillator
}

object OscillatorFactory {
  val CODEC: Codec[OscillatorFactory] = Kind.CODEC.dispatch(
    _.kind,
    {
      case Kind.Zero => Zero.CODEC
      case Kind.Adsr => Adsr.CODEC
      case Kind.AliasSine => AliasSine.CODEC
      case Kind.AliasSquare => AliasSquare.CODEC
      case Kind.AliasSawtooth => AliasSawtooth.CODEC
      case Kind.AliasTriangle => AliasTriangle.CODEC
      case Kind.SampleAndHoldNoise => SampleAndHoldNoise.CODEC
      case Kind.LfsrNoise => LfsrNoise.CODEC
      case Kind.BlepSquare => BlepSquare.CODEC
      case Kind.BlepSawtooth => BlepSawtooth.CODEC
      case Kind.BlepTriangle => BlepTriangle.CODEC
      case Kind.Sine => Sine.CODEC
      case Kind.WavetableSampler => WavetableSampler.CODEC
      case _ => Zero.CODEC
    },
  )

  val STREAM_CODEC: StreamCodec[ByteBuf, OscillatorFactory] = Kind.STREAM_CODEC.dispatch(
    _.kind,
    {
      case Kind.Zero => Zero.STREAM_CODEC
      case Kind.Adsr => Adsr.STREAM_CODEC
      case Kind.AliasSine => AliasSine.STREAM_CODEC
      case Kind.AliasSquare => AliasSquare.STREAM_CODEC
      case Kind.AliasSawtooth => AliasSawtooth.STREAM_CODEC
      case Kind.AliasTriangle => AliasTriangle.STREAM_CODEC
      case Kind.SampleAndHoldNoise => SampleAndHoldNoise.STREAM_CODEC
      case Kind.LfsrNoise => LfsrNoise.STREAM_CODEC
      case Kind.BlepSquare => BlepSquare.STREAM_CODEC
      case Kind.BlepSawtooth => BlepSawtooth.STREAM_CODEC
      case Kind.BlepTriangle => BlepTriangle.STREAM_CODEC
      case Kind.Sine => Sine.STREAM_CODEC
      case Kind.WavetableSampler => WavetableSampler.STREAM_CODEC
      case _ => Zero.STREAM_CODEC
    },
  )

  object Kind extends Enumeration with ScalaCodec.Enumeration with ScalaStreamCodec.Enumeration {
    val Zero: Value = Value
    val Adsr: Value = Value
    val AliasSine: Value = Value
    val AliasSquare: Value = Value
    val AliasSawtooth: Value = Value
    val AliasTriangle: Value = Value
    val SampleAndHoldNoise: Value = Value
    val LfsrNoise: Value = Value
    val BlepSquare: Value = Value
    val BlepSawtooth: Value = Value
    val BlepTriangle: Value = Value
    val Sine: Value = Value
    val WavetableSampler: Value = Value
  }

  case object Zero extends OscillatorFactory {
    override def kind: Kind.Value = Kind.Zero

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = new ZeroOscillator(model)

    val CODEC: MapCodec[Zero.type] = MapCodec.unit(Zero)
    val STREAM_CODEC: StreamCodec[ByteBuf, Zero.type] = StreamCodec.unit(Zero)
  }

  case class Adsr(
    attack: ParameterValue,
    decay: ParameterValue,
    sustain: ParameterValue,
    release: ParameterValue,
  ) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.Adsr

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      val osc = new AdsrOscillator(model)
      attack.set(interp.synth, osc.Attack)
      decay.set(interp.synth, osc.Decay)
      sustain.set(interp.synth, osc.Sustain)
      release.set(interp.synth, osc.Release)

      osc
    }
  }

  object Adsr {
    val CODEC: MapCodec[Adsr] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ParameterValue.CODEC.fieldOf("attack").forGetter(_.attack),
          ParameterValue.CODEC.fieldOf("decay").forGetter(_.decay),
          ParameterValue.CODEC.fieldOf("sustain").forGetter(_.sustain),
          ParameterValue.CODEC.fieldOf("release").forGetter(_.release),
        )
        .apply(inst, Adsr.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, Adsr] = StreamCodec.composite(
      ParameterValue.STREAM_CODEC,
      _.attack,
      ParameterValue.STREAM_CODEC,
      _.decay,
      ParameterValue.STREAM_CODEC,
      _.sustain,
      ParameterValue.STREAM_CODEC,
      _.release,
      Adsr.apply _,
    )
  }

  case class AliasSine(unipolar: Boolean) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.AliasSine

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      new AliasingOscillator.Sine(model, unipolar)
    }
  }

  object AliasSine {
    val CODEC: MapCodec[AliasSine] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ScalaCodec.BOOL.fieldOf("unipolar").forGetter(_.unipolar))
        .apply(inst, AliasSine(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, AliasSine] = ScalaStreamCodec.BOOL.map(AliasSine(_), _.unipolar)
  }

  case class AliasSquare(unipolar: Boolean) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.AliasSquare

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      new AliasingOscillator.Square(model, unipolar)
    }
  }

  object AliasSquare {
    val CODEC: MapCodec[AliasSquare] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ScalaCodec.BOOL.fieldOf("unipolar").forGetter(_.unipolar))
        .apply(inst, AliasSquare(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, AliasSquare] = ScalaStreamCodec.BOOL.map(AliasSquare(_), _.unipolar)
  }

  case class AliasSawtooth(unipolar: Boolean) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.AliasSawtooth

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      new AliasingOscillator.Sawtooth(model, unipolar)
    }
  }

  object AliasSawtooth {
    val CODEC: MapCodec[AliasSawtooth] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ScalaCodec.BOOL.fieldOf("unipolar").forGetter(_.unipolar))
        .apply(inst, AliasSawtooth(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, AliasSawtooth] = ScalaStreamCodec.BOOL.map(AliasSawtooth(_), _.unipolar)
  }

  case class AliasTriangle(unipolar: Boolean) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.AliasTriangle

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      new AliasingOscillator.Triangle(model, unipolar)
    }
  }

  object AliasTriangle {
    val CODEC: MapCodec[AliasTriangle] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ScalaCodec.BOOL.fieldOf("unipolar").forGetter(_.unipolar))
        .apply(inst, AliasTriangle(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, AliasTriangle] = ScalaStreamCodec.BOOL.map(AliasTriangle(_), _.unipolar)
  }

  case class SampleAndHoldNoise(unipolar: Boolean) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.SampleAndHoldNoise

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      new AliasingOscillator.SampleAndHoldNoise(model, unipolar)
    }
  }

  object SampleAndHoldNoise {
    val CODEC: MapCodec[SampleAndHoldNoise] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ScalaCodec.BOOL.fieldOf("unipolar").forGetter(_.unipolar))
        .apply(inst, SampleAndHoldNoise(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SampleAndHoldNoise] = {
      ScalaStreamCodec.BOOL.map(SampleAndHoldNoise(_), _.unipolar)
    }
  }

  case class LfsrNoise(initial: Int, mask: Int, unipolar: Boolean) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.LfsrNoise

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      new AliasingOscillator.LfsrNoise(model, initial, mask, unipolar)
    }
  }

  object LfsrNoise {
    val CODEC: MapCodec[LfsrNoise] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ScalaCodec.INT.fieldOf("initial").forGetter(_.initial),
          ScalaCodec.INT.fieldOf("mask").forGetter(_.mask),
          ScalaCodec.BOOL.fieldOf("unipolar").forGetter(_.unipolar),
        )
        .apply(inst, LfsrNoise.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, LfsrNoise] = StreamCodec.composite(
      ScalaStreamCodec.INT,
      _.initial,
      ScalaStreamCodec.INT,
      _.mask,
      ScalaStreamCodec.BOOL,
      _.unipolar,
      LfsrNoise.apply _,
    )
  }

  case class BlepSquare(unipolar: Boolean) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.BlepSquare

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      new BlepOscillator.Square(model, unipolar)
    }
  }

  object BlepSquare {
    val CODEC: MapCodec[BlepSquare] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ScalaCodec.BOOL.fieldOf("unipolar").forGetter(_.unipolar))
        .apply(inst, BlepSquare.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, BlepSquare] = ScalaStreamCodec.BOOL.map(BlepSquare(_), _.unipolar)
  }

  case class BlepSawtooth(unipolar: Boolean) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.BlepSawtooth

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      new BlepOscillator.Sawtooth(model, unipolar)
    }
  }

  object BlepSawtooth {
    val CODEC: MapCodec[BlepSawtooth] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ScalaCodec.BOOL.fieldOf("unipolar").forGetter(_.unipolar))
        .apply(inst, BlepSawtooth.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, BlepSawtooth] = ScalaStreamCodec.BOOL.map(BlepSawtooth(_), _.unipolar)
  }

  case class BlepTriangle(unipolar: Boolean) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.BlepTriangle

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      new BlepOscillator.Triangle(model, unipolar)
    }
  }

  object BlepTriangle {
    val CODEC: MapCodec[BlepTriangle] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ScalaCodec.BOOL.fieldOf("unipolar").forGetter(_.unipolar))
        .apply(inst, BlepTriangle.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, BlepTriangle] = ScalaStreamCodec.BOOL.map(BlepTriangle(_), _.unipolar)
  }

  case class Sine(unipolar: Boolean) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.Sine

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      new SineOscillator(model, unipolar)
    }
  }

  object Sine {
    val CODEC: MapCodec[Sine] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ScalaCodec.BOOL.fieldOf("unipolar").forGetter(_.unipolar))
        .apply(inst, Sine(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, Sine] = ScalaStreamCodec.BOOL.map(Sine(_), _.unipolar)
  }

  case class WavetableSampler(morph: ParameterValue) extends OscillatorFactory {
    override def kind: Kind.Value = Kind.WavetableSampler

    override def make(interp: Interpreter, model: OscillatorModel): Oscillator = {
      val osc = new WavetableOscillator(model)
      morph.set(interp.synth, osc.Morph)

      osc
    }
  }

  object WavetableSampler {
    val CODEC: MapCodec[WavetableSampler] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ParameterValue.CODEC.fieldOf("morph").forGetter(_.morph))
        .apply(inst, WavetableSampler(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, WavetableSampler] = {
      ParameterValue.STREAM_CODEC.map(WavetableSampler(_), _.morph)
    }
  }
}
