package li.cil.oc.common.audio.synth.instr

import com.mojang.serialization.{Codec, MapCodec}
import io.netty.buffer.ByteBuf
import li.cil.oc.common.audio.synth.waveshaper.ShapingFunction
import li.cil.oc.common.datacomponents.{ScalaCodec, ScalaStreamCodec}
import net.minecraft.network.codec.StreamCodec

trait ShaperFactory {
  def kind: ShaperFactory.Kind.Value

  def make(interp: Interpreter): ShapingFunction
}

object ShaperFactory {
  val CODEC: Codec[ShaperFactory] = Kind.CODEC.dispatch(
    _.kind,
    {
      case Kind.Identity => Identity.CODEC
      case Kind.SoftClip => SoftClip.CODEC
      case Kind.HardClip => HardClip.CODEC
      case _ => Identity.CODEC
    }
  )

  val STREAM_CODEC: StreamCodec[ByteBuf, ShaperFactory] = Kind.STREAM_CODEC.dispatch(
    _.kind,
    {
      case Kind.Identity => Identity.STREAM_CODEC
      case Kind.SoftClip => SoftClip.STREAM_CODEC
      case Kind.HardClip => HardClip.STREAM_CODEC
      case _ => Identity.STREAM_CODEC
    }
  )

  object Kind extends Enumeration with ScalaCodec.Enumeration with ScalaStreamCodec.Enumeration {
    val Identity: Value = Value
    val SoftClip: Value = Value
    val HardClip: Value = Value
  }

  private abstract class SingletonFactory(
    override val kind: Kind.Value,
    shaper: ShapingFunction
  ) extends ShaperFactory {
    override def make(interp: Interpreter): ShapingFunction = shaper
  }

  object Identity extends SingletonFactory(Kind.Identity, ShapingFunction.Identity) {
    val CODEC: MapCodec[this.type] = MapCodec.unit(this)
    val STREAM_CODEC: StreamCodec[ByteBuf, this.type] = StreamCodec.unit(this)
  }

  object SoftClip extends SingletonFactory(Kind.SoftClip, ShapingFunction.SoftClip) {
    val CODEC: MapCodec[this.type] = MapCodec.unit(this)
    val STREAM_CODEC: StreamCodec[ByteBuf, this.type] = StreamCodec.unit(this)
  }

  object HardClip extends SingletonFactory(Kind.HardClip, ShapingFunction.HardClip) {
    val CODEC: MapCodec[this.type] = MapCodec.unit(this)
    val STREAM_CODEC: StreamCodec[ByteBuf, this.type] = StreamCodec.unit(this)
  }
}
