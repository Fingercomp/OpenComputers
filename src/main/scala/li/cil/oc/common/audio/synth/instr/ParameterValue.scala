package li.cil.oc.common.audio.synth.instr

import com.mojang.serialization.{Codec, MapCodec}
import com.mojang.serialization.codecs.RecordCodecBuilder
import io.netty.buffer.ByteBuf
import li.cil.oc.common.audio.synth.Synth
import li.cil.oc.common.audio.synth.channel.ChannelId
import li.cil.oc.common.audio.synth.parameter.Parameter
import li.cil.oc.common.datacomponents.{ScalaCodec, ScalaStreamCodec}
import net.minecraft.network.codec.StreamCodec

sealed trait ParameterValue {
  def kind: ParameterValue.Kind.Value

  def set(synth: Synth, parameter: Parameter): Unit
}

object ParameterValue {
  object Kind extends Enumeration {
    val Default, Constant, Adsr, Channel = Value
  }

  val CODEC: Codec[ParameterValue] = ScalaCodec.BYTE.dispatch(
    _.kind.id.toByte,
    id => {
      Kind(id) match {
        case Kind.Default => Default.CODEC
        case Kind.Constant => Constant.CODEC
        case Kind.Adsr => Adsr.CODEC
        case Kind.Channel => Channel.CODEC
        case _ => Default.CODEC
      }
    },
  )

  val STREAM_CODEC: StreamCodec[ByteBuf, ParameterValue] = ScalaStreamCodec.BYTE.dispatch(
    _.kind.id.toByte,
    id => {
      Kind(id) match {
        case Kind.Default => Default.STREAM_CODEC
        case Kind.Constant => Constant.STREAM_CODEC
        case Kind.Adsr => Adsr.STREAM_CODEC
        case Kind.Channel => Channel.STREAM_CODEC
        case _ => Default.STREAM_CODEC
      }
    }
  )

  case object Default extends ParameterValue {
    override def kind: Kind.Value = Kind.Default

    override def set(synth: Synth, parameter: Parameter): Unit = {
      parameter.reset()
    }

    val CODEC: MapCodec[Default.type] = MapCodec.unit(Default)
    val STREAM_CODEC: StreamCodec[ByteBuf, Default.type] = StreamCodec.unit(Default)
  }

  case class Constant(value: Float) extends ParameterValue {
    override def kind: Kind.Value = Kind.Constant

    override def set(synth: Synth, parameter: Parameter): Unit = {
      parameter.setConstant(value)
    }
  }

  object Constant {
    val CODEC: MapCodec[Constant] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ScalaCodec.FLOAT.fieldOf("value").forGetter(_.value))
        .apply(inst, Constant(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, Constant] = ScalaStreamCodec.FLOAT.map(
      Constant(_),
      _.value
    )
  }

  case class Adsr(
                   var attack: Float,
                   var decay: Float,
                   var sustain: Float,
                   var release: Float,
                   var low: Float,
                   var high: Float,
                 ) extends ParameterValue {
    override def kind: Kind.Value = Kind.Adsr

    override def set(synth: Synth, parameter: Parameter): Unit = {
      parameter.setAdsr(attack, decay, sustain, release, low, high)
    }
  }

  object Adsr {
    val CODEC: MapCodec[Adsr] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ScalaCodec.FLOAT.fieldOf("attack").forGetter(_.attack),
          ScalaCodec.FLOAT.fieldOf("decay").forGetter(_.decay),
          ScalaCodec.FLOAT.fieldOf("sustain").forGetter(_.sustain),
          ScalaCodec.FLOAT.fieldOf("release").forGetter(_.release),
          ScalaCodec.FLOAT.fieldOf("low").forGetter(_.low),
          ScalaCodec.FLOAT.fieldOf("high").forGetter(_.high),
        )
        .apply(inst, Adsr.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, Adsr] = StreamCodec.composite(
      ScalaStreamCodec.FLOAT, _.attack,
      ScalaStreamCodec.FLOAT, _.decay,
      ScalaStreamCodec.FLOAT, _.sustain,
      ScalaStreamCodec.FLOAT, _.release,
      ScalaStreamCodec.FLOAT, _.low,
      ScalaStreamCodec.FLOAT, _.high,
      Adsr.apply _,
    )
  }

  case class Channel(var sourceChannelId: ChannelId) extends ParameterValue {
    override def kind: Kind.Value = Kind.Channel

    override def set(synth: Synth, parameter: Parameter): Unit = {
      for (channel <- synth.channelById(sourceChannelId)) {
        parameter.modulateBy(channel)
      }
    }
  }

  object Channel {
    val CODEC: MapCodec[Channel] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ChannelId.CODEC.fieldOf("source").forGetter(_.sourceChannelId))
        .apply(inst, Channel(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, Channel] = ChannelId.STREAM_CODEC.map(Channel(_), _.sourceChannelId)
  }
}
