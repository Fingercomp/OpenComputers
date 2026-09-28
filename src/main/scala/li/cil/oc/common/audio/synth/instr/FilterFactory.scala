package li.cil.oc.common.audio.synth.instr

import com.mojang.serialization.{Codec, MapCodec}
import io.netty.buffer.ByteBuf
import li.cil.oc.common.audio.synth.channel.Channel
import li.cil.oc.common.audio.synth.filter.{ChannelFilter, SVFilter}
import li.cil.oc.common.datacomponents.{ScalaCodec, ScalaStreamCodec}
import net.minecraft.network.codec.StreamCodec

trait FilterFactory {
  def kind: FilterFactory.Kind.Value

  def make(interp: Interpreter, channel: Channel): ChannelFilter
}

object FilterFactory {
  val CODEC: Codec[Option[FilterFactory]] = Kind.CODEC.dispatch(
    _.map(_.kind).getOrElse(Kind.None),
    {
      case Kind.None => MapCodec.unit(None)
      case Kind.LowPass12dB => LowPass12dB.CODEC.xmap(Some(_), _ => LowPass12dB)
      case Kind.BandPass12dB => BandPass12dB.CODEC.xmap(Some(_), _ => BandPass12dB)
      case Kind.HighPass12dB => HighPass12dB.CODEC.xmap(Some(_), _ => HighPass12dB)
      case Kind.LowPass24dB => LowPass24dB.CODEC.xmap(Some(_), _ => LowPass24dB)
      case Kind.BandPass24dB => BandPass24dB.CODEC.xmap(Some(_), _ => BandPass24dB)
      case Kind.HighPass24dB => HighPass24dB.CODEC.xmap(Some(_), _ => HighPass24dB)
      case _ => MapCodec.unit(None)
    },
  )

  val STREAM_CODEC: StreamCodec[ByteBuf, Option[FilterFactory]] = Kind.STREAM_CODEC.dispatch(
    _.map(_.kind).getOrElse(Kind.None),
    {
      case Kind.None => StreamCodec.unit(None)
      case Kind.LowPass12dB => LowPass12dB.STREAM_CODEC.map(Some(_), _ => LowPass12dB)
      case Kind.BandPass12dB => BandPass12dB.STREAM_CODEC.map(Some(_), _ => BandPass12dB)
      case Kind.HighPass12dB => HighPass12dB.STREAM_CODEC.map(Some(_), _ => HighPass12dB)
      case Kind.LowPass24dB => LowPass24dB.STREAM_CODEC.map(Some(_), _ => LowPass24dB)
      case Kind.BandPass24dB => BandPass24dB.STREAM_CODEC.map(Some(_), _ => BandPass24dB)
      case Kind.HighPass24dB => HighPass24dB.STREAM_CODEC.map(Some(_), _ => HighPass24dB)
      case _ => StreamCodec.unit(None)
    },
  )

  object Kind extends Enumeration with ScalaCodec.Enumeration with ScalaStreamCodec.Enumeration {
    val None: Value = Value
    val LowPass12dB: Value = Value
    val BandPass12dB: Value = Value
    val HighPass12dB: Value = Value
    val LowPass24dB: Value = Value
    val BandPass24dB: Value = Value
    val HighPass24dB: Value = Value
  }

  private abstract class FactorySingleton(
    override val kind: Kind.Value,
    ctor: Channel => ChannelFilter,
  ) extends FilterFactory {
    override def make(interp: Interpreter, channel: Channel): ChannelFilter = ctor(channel)
  }

  object LowPass12dB extends FactorySingleton(Kind.LowPass12dB, new SVFilter.LowPass12dB(_)) {
    val CODEC: MapCodec[this.type] = MapCodec.unit(this)
    val STREAM_CODEC: StreamCodec[ByteBuf, this.type] = StreamCodec.unit(this)
  }

  object BandPass12dB extends FactorySingleton(Kind.BandPass12dB, new SVFilter.BandPass12dB(_)) {
    val CODEC: MapCodec[this.type] = MapCodec.unit(this)
    val STREAM_CODEC: StreamCodec[ByteBuf, this.type] = StreamCodec.unit(this)
  }

  object HighPass12dB extends FactorySingleton(Kind.BandPass12dB, new SVFilter.HighPass12dB(_)) {
    val CODEC: MapCodec[this.type] = MapCodec.unit(this)
    val STREAM_CODEC: StreamCodec[ByteBuf, this.type] = StreamCodec.unit(this)
  }

  object LowPass24dB extends FactorySingleton(Kind.LowPass24dB, new SVFilter.LowPass24dB(_)) {
    val CODEC: MapCodec[this.type] = MapCodec.unit(this)
    val STREAM_CODEC: StreamCodec[ByteBuf, this.type] = StreamCodec.unit(this)
  }

  object BandPass24dB extends FactorySingleton(Kind.BandPass24dB, new SVFilter.BandPass24dB(_)) {
    val CODEC: MapCodec[this.type] = MapCodec.unit(this)
    val STREAM_CODEC: StreamCodec[ByteBuf, this.type] = StreamCodec.unit(this)
  }

  object HighPass24dB extends FactorySingleton(Kind.BandPass24dB, new SVFilter.HighPass24dB(_)) {
    val CODEC: MapCodec[this.type] = MapCodec.unit(this)
    val STREAM_CODEC: StreamCodec[ByteBuf, this.type] = StreamCodec.unit(this)
  }
}
