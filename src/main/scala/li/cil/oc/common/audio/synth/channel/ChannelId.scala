package li.cil.oc.common.audio.synth.channel

import com.mojang.serialization.{Codec, MapCodec}
import com.mojang.serialization.codecs.RecordCodecBuilder
import io.netty.buffer.ByteBuf
import li.cil.oc.common.datacomponents.{ScalaCodec, ScalaStreamCodec}
import net.minecraft.network.codec.{ByteBufCodecs, StreamCodec}

sealed trait ChannelId

object ChannelId {
  case class Voice(idx: Int) extends ChannelId

  object Voice {
    val CODEC: MapCodec[Voice] = RecordCodecBuilder.mapCodec(inst => {
      inst.group(ScalaCodec.INT.fieldOf("idx").forGetter(_.idx)).apply(inst, Voice(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, Voice] = ByteBufCodecs.VAR_INT.map(Voice(_), _.idx)
  }

  case object Master extends ChannelId {
    val CODEC: MapCodec[Master.type] = MapCodec.unit(this)
    val STREAM_CODEC: StreamCodec[ByteBuf, Master.type] = StreamCodec.unit(this)
  }

  case class Lfo(idx: Int) extends ChannelId

  object Lfo {
    val CODEC: MapCodec[Lfo] = RecordCodecBuilder.mapCodec(inst => {
      inst.group(ScalaCodec.INT.fieldOf("idx").forGetter(_.idx)).apply(inst, Lfo(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, Lfo] = ByteBufCodecs.VAR_INT.map(Lfo(_), _.idx)
  }

  private object Kind extends Enumeration with ScalaCodec.Enumeration with ScalaStreamCodec.Enumeration {
    val Voice, Master, Lfo = Value
  }

  private def kind(id: ChannelId): Kind.Value = id match {
    case Voice(_) => Kind.Voice
    case Master => Kind.Master
    case Lfo(_) => Kind.Lfo
  }

  val CODEC: Codec[ChannelId] = Kind.CODEC.dispatch(kind, {
    case Kind.Voice => Voice.CODEC
    case Kind.Master => Master.CODEC
    case Kind.Lfo => Lfo.CODEC
    case _ => MapCodec.unit(Voice(0))
  })

  val STREAM_CODEC: StreamCodec[ByteBuf, ChannelId] = Kind.STREAM_CODEC.dispatch(kind, {
    case Kind.Voice => Voice.STREAM_CODEC
    case Kind.Master => Master.STREAM_CODEC
    case Kind.Lfo => Lfo.STREAM_CODEC
    case _ => StreamCodec.unit(Voice(0))
  })
}
