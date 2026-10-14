package li.cil.oc.common.audio.synth.instr

import com.mojang.serialization.codecs.RecordCodecBuilder
import com.mojang.serialization.{Codec, MapCodec}
import io.netty.buffer.ByteBuf
import li.cil.oc.common.audio.synth.channel.{ChannelId, FilteredChannel, OscillatorChannel, VoiceChannel}
import li.cil.oc.common.audio.synth.oscillator.WavetableOscillator.Wavetable
import li.cil.oc.common.audio.synth.waveshaper.ShapingFunction
import li.cil.oc.common.datacomponents.{ScalaCodec, ScalaStreamCodec}
import net.minecraft.network.codec.{ByteBufCodecs, StreamCodec}

import java.nio.{ByteBuffer, FloatBuffer}

sealed trait Instruction {
  def opcode: Instruction.Opcode.Value

  def execute(interp: Interpreter): Unit
}

object Instruction {
  object Opcode extends Enumeration with ScalaCodec.Enumeration with ScalaStreamCodec.Enumeration {
    val Nop: Value = Value
    val Open: Value = Value
    val Close: Value = Value
    val SetFrequency: Value = Value
    val Delay: Value = Value
    val SetAdsr: Value = Value
    val ResetEnvelope: Value = Value
    val SetFm: Value = Value
    val ResetFm: Value = Value
    val SetAm: Value = Value
    val ResetAm: Value = Value
    val SetSum: Value = Value
    val ResetSum: Value = Value
    val SetOscillator: Value = Value
    val SetFilter: Value = Value
    val ResetFilter: Value = Value
    val SetWaveshaper: Value = Value
    val ResetWaveshaper: Value = Value
    val SetSync: Value = Value
    val SetFeedback: Value = Value
    val SetOscillatorLevel: Value = Value
    val SetChannelOutputGain: Value = Value
    val SetMasterSendLevel: Value = Value
    val LoadWavetable: Value = Value
  }

  val CODEC: Codec[Instruction] = Opcode.CODEC.dispatch(
    "opcode",
    _.opcode,
    {
      case Opcode.Nop => Nop.CODEC
      case Opcode.Open => Open.CODEC
      case Opcode.Close => Close.CODEC
      case Opcode.SetFrequency => SetFrequency.CODEC
      case Opcode.Delay => Delay.CODEC
      case Opcode.SetAdsr => SetAdsr.CODEC
      case Opcode.ResetEnvelope => ResetEnvelope.CODEC
      case Opcode.SetFm => SetFm.CODEC
      case Opcode.ResetFm => ResetFm.CODEC
      case Opcode.SetAm => SetAm.CODEC
      case Opcode.ResetAm => ResetAm.CODEC
      case Opcode.SetSum => SetAm.CODEC
      case Opcode.ResetSum => ResetSum.CODEC
      case Opcode.SetOscillator => SetOscillator.CODEC
      case Opcode.SetFilter => SetFilter.CODEC
      case Opcode.ResetFilter => ResetFilter.CODEC
      case Opcode.SetWaveshaper => SetWaveshaper.CODEC
      case Opcode.ResetWaveshaper => ResetWaveshaper.CODEC
      case Opcode.SetSync => SetSync.CODEC
      case Opcode.SetFeedback => SetFeedback.CODEC
      case Opcode.SetOscillatorLevel => SetOscillatorLevel.CODEC
      case Opcode.SetChannelOutputGain => SetChannelOutputGain.CODEC
      case Opcode.SetMasterSendLevel => SetMasterSendLevel.CODEC
      case Opcode.LoadWavetable => LoadWavetable.CODEC
      case _ => Nop.CODEC
    },
  )

  val STREAM_CODEC: StreamCodec[ByteBuf, Instruction] = Opcode.STREAM_CODEC.dispatch(
    _.opcode,
    {
      case Opcode.Nop => Nop.STREAM_CODEC
      case Opcode.Open => Open.STREAM_CODEC
      case Opcode.Close => Close.STREAM_CODEC
      case Opcode.SetFrequency => SetFrequency.STREAM_CODEC
      case Opcode.Delay => Delay.STREAM_CODEC
      case Opcode.SetAdsr => SetAdsr.STREAM_CODEC
      case Opcode.ResetEnvelope => ResetEnvelope.STREAM_CODEC
      case Opcode.SetFm => SetFm.STREAM_CODEC
      case Opcode.ResetFm => ResetFm.STREAM_CODEC
      case Opcode.SetAm => SetAm.STREAM_CODEC
      case Opcode.ResetAm => ResetAm.STREAM_CODEC
      case Opcode.SetSum => SetSum.STREAM_CODEC
      case Opcode.ResetSum => ResetSum.STREAM_CODEC
      case Opcode.SetOscillator => SetOscillator.STREAM_CODEC
      case Opcode.SetFilter => SetFilter.STREAM_CODEC
      case Opcode.ResetFilter => ResetFilter.STREAM_CODEC
      case Opcode.SetWaveshaper => SetWaveshaper.STREAM_CODEC
      case Opcode.ResetWaveshaper => ResetWaveshaper.STREAM_CODEC
      case Opcode.SetSync => SetSync.STREAM_CODEC
      case Opcode.SetFeedback => SetFeedback.STREAM_CODEC
      case Opcode.SetOscillatorLevel => SetOscillatorLevel.STREAM_CODEC
      case Opcode.SetChannelOutputGain => SetChannelOutputGain.STREAM_CODEC
      case Opcode.SetMasterSendLevel => SetMasterSendLevel.STREAM_CODEC
      case Opcode.LoadWavetable => LoadWavetable.STREAM_CODEC
      case _ => Nop.STREAM_CODEC
    },
  )

  case object Nop extends Instruction {
    override def opcode: Opcode.Value = Opcode.Nop

    override def execute(interp: Interpreter): Unit = {}

    val CODEC: MapCodec[Nop.type] = MapCodec.unit(Nop)
    val STREAM_CODEC: StreamCodec[ByteBuf, Nop.type] = StreamCodec.unit(Nop)
  }

  case class Open(channelId: ChannelId) extends Instruction {
    override def opcode: Opcode.Value = Opcode.Open

    override def execute(interp: Interpreter): Unit = {
      interp.synth.channelById(channelId).foreach(_.open())
    }
  }

  object Open {
    val CODEC: MapCodec[Open] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId))
        .apply(inst, Open(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, Open] = ChannelId.STREAM_CODEC.map(Open(_), _.channelId)
  }

  case class Close(channelId: ChannelId) extends Instruction {
    override def opcode: Opcode.Value = Opcode.Close

    override def execute(interp: Interpreter): Unit = {
      interp.synth.channelById(channelId).foreach(_.close())
    }
  }

  object Close {
    val CODEC: MapCodec[Close] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId))
        .apply(inst, Close(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, Close] = ChannelId.STREAM_CODEC.map(Close(_), _.channelId)
  }

  case class SetFrequency(
    channelId: ChannelId,
    frequency: Float,
  ) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetFrequency

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        channel.asInstanceOf[OscillatorChannel].frequency = frequency
      }
    }
  }

  object SetFrequency {
    val CODEC: MapCodec[SetFrequency] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId),
          ScalaCodec.FLOAT.fieldOf("frequency").forGetter(_.frequency),
        )
        .apply(inst, SetFrequency.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetFrequency] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.channelId,
      ScalaStreamCodec.FLOAT,
      _.frequency,
      SetFrequency.apply _,
    )
  }

  case class Delay(duration: Float) extends Instruction {
    override def opcode: Opcode.Value = Opcode.Delay

    override def execute(interp: Interpreter): Unit = {
      interp.delay(duration)
    }
  }

  object Delay {
    val CODEC: MapCodec[Delay] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ScalaCodec.FLOAT.fieldOf("duration").forGetter(_.duration))
        .apply(inst, Delay(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, Delay] = ScalaStreamCodec.FLOAT.map(Delay(_), _.duration)
  }

  case class SetAdsr(
    channelId: ChannelId,
    attack: Float,
    decay: Float,
    sustain: Float,
    release: Float,
  ) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetAdsr

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        val envelope = channel.asInstanceOf[OscillatorChannel].envelope
        envelope.Attack.setConstant(attack)
        envelope.Decay.setConstant(decay)
        envelope.Sustain.setConstant(sustain)
        envelope.Release.setConstant(release)
      }
    }
  }

  object SetAdsr {
    val CODEC: MapCodec[SetAdsr] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId),
          ScalaCodec.FLOAT.fieldOf("attack").forGetter(_.attack),
          ScalaCodec.FLOAT.fieldOf("decay").forGetter(_.decay),
          ScalaCodec.FLOAT.fieldOf("sustain").forGetter(_.sustain),
          ScalaCodec.FLOAT.fieldOf("release").forGetter(_.release),
        )
        .apply(inst, SetAdsr.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetAdsr] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.channelId,
      ScalaStreamCodec.FLOAT,
      _.attack,
      ScalaStreamCodec.FLOAT,
      _.decay,
      ScalaStreamCodec.FLOAT,
      _.sustain,
      ScalaStreamCodec.FLOAT,
      _.release,
      SetAdsr.apply _,
    )
  }

  case class ResetEnvelope(channelId: ChannelId) extends Instruction {
    override def opcode: Opcode.Value = Opcode.ResetEnvelope

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        channel.asInstanceOf[OscillatorChannel].envelope.reset()
      }
    }
  }

  object ResetEnvelope {
    val CODEC: MapCodec[ResetEnvelope] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId))
        .apply(inst, ResetEnvelope(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, ResetEnvelope] = ChannelId.STREAM_CODEC.map(ResetEnvelope(_), _.channelId)
  }

  case class SetFm(
    targetChannelId: ChannelId,
    sourceChannelId: ChannelId,
    modIndex: ParameterValue,
  ) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetFm

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(targetChannelId)) {
        val modulation = channel.asInstanceOf[VoiceChannel].fm.addInput(sourceChannelId)
        modIndex.set(interp.synth, modulation.ModIndex)
      }
    }
  }

  object SetFm {
    val CODEC: MapCodec[SetFm] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("target").forGetter(_.targetChannelId),
          ChannelId.CODEC.fieldOf("source").forGetter(_.sourceChannelId),
          ParameterValue.CODEC.fieldOf("mod_index").forGetter(_.modIndex),
        )
        .apply(inst, SetFm.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetFm] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.targetChannelId,
      ChannelId.STREAM_CODEC,
      _.sourceChannelId,
      ParameterValue.STREAM_CODEC,
      _.modIndex,
      SetFm.apply _,
    )
  }

  case class ResetFm(targetChannelId: ChannelId, sourceChannelId: Option[ChannelId]) extends Instruction {
    override def opcode: Opcode.Value = Opcode.ResetFm

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(targetChannelId)) {
        val fm = channel.asInstanceOf[VoiceChannel].fm

        sourceChannelId match {
          case Some(sourceChannelId) => fm.removeInput(sourceChannelId)
          case None => fm.reset()
        }
      }
    }
  }

  object ResetFm {
    val CODEC: MapCodec[ResetFm] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("target").forGetter(_.targetChannelId),
          ScalaCodec.optionFieldOf("source", ChannelId.CODEC).forGetter(_.sourceChannelId),
        )
        .apply(inst, ResetFm.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, ResetFm] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.targetChannelId,
      ScalaStreamCodec.option(ChannelId.STREAM_CODEC),
      _.sourceChannelId,
      ResetFm.apply _,
    )
  }

  case class SetAm(
    targetChannelId: ChannelId,
    sourceChannelId: ChannelId,
    modIndex: ParameterValue,
  ) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetAm

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(targetChannelId)) {
        val modulation = channel.asInstanceOf[VoiceChannel].am.addInput(sourceChannelId)
        modIndex.set(interp.synth, modulation.ModIndex)
      }
    }
  }

  object SetAm {
    val CODEC: MapCodec[SetAm] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("target").forGetter(_.targetChannelId),
          ChannelId.CODEC.fieldOf("source").forGetter(_.sourceChannelId),
          ParameterValue.CODEC.fieldOf("mod_index").forGetter(_.modIndex),
        )
        .apply(inst, SetAm.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetAm] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.targetChannelId,
      ChannelId.STREAM_CODEC,
      _.sourceChannelId,
      ParameterValue.STREAM_CODEC,
      _.modIndex,
      SetAm.apply _,
    )
  }

  case class ResetAm(targetChannelId: ChannelId, sourceChannelId: Option[ChannelId]) extends Instruction {
    override def opcode: Opcode.Value = Opcode.ResetAm

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(targetChannelId)) {
        val am = channel.asInstanceOf[VoiceChannel].am

        sourceChannelId match {
          case Some(sourceChannelId) => am.removeInput(sourceChannelId)
          case None => am.reset()
        }
      }
    }
  }

  object ResetAm {
    val CODEC: MapCodec[ResetAm] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("target").forGetter(_.targetChannelId),
          ScalaCodec.optionFieldOf("source", ChannelId.CODEC).forGetter(_.sourceChannelId),
        )
        .apply(inst, ResetAm.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, ResetAm] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.targetChannelId,
      ScalaStreamCodec.option(ChannelId.STREAM_CODEC),
      _.sourceChannelId,
      ResetAm.apply _,
    )
  }

  case class SetSum(
    sourceChannelId: ChannelId,
    targetChannelId: ChannelId,
    level: ParameterValue,
  ) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetSum

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(targetChannelId)) {
        val modulation = channel.asInstanceOf[VoiceChannel].sum.addInput(sourceChannelId)
        level.set(interp.synth, modulation.Level)
      }
    }
  }

  object SetSum {
    val CODEC: MapCodec[SetSum] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("target").forGetter(_.targetChannelId),
          ChannelId.CODEC.fieldOf("source").forGetter(_.sourceChannelId),
          ParameterValue.CODEC.fieldOf("level").forGetter(_.level),
        )
        .apply(inst, SetSum.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetSum] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.targetChannelId,
      ChannelId.STREAM_CODEC,
      _.sourceChannelId,
      ParameterValue.STREAM_CODEC,
      _.level,
      SetSum.apply _,
    )
  }

  case class ResetSum(targetChannelId: ChannelId, sourceChannelId: Option[ChannelId]) extends Instruction {
    override def opcode: Opcode.Value = Opcode.ResetSum

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(targetChannelId)) {
        val sum = channel.asInstanceOf[VoiceChannel].sum

        sourceChannelId match {
          case Some(sourceChannelId) => sum.removeInput(sourceChannelId)
          case None => sum.reset()
        }
      }
    }
  }

  object ResetSum {
    val CODEC: MapCodec[ResetSum] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("target").forGetter(_.targetChannelId),
          ScalaCodec.optionFieldOf("source", ChannelId.CODEC).forGetter(_.sourceChannelId),
        )
        .apply(inst, ResetSum.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, ResetSum] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.targetChannelId,
      ScalaStreamCodec.option(ChannelId.STREAM_CODEC),
      _.sourceChannelId,
      ResetSum.apply _,
    )
  }

  case class SetOscillator(channelId: ChannelId, oscillator: OscillatorFactory) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetOscillator

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        val oscCh = channel.asInstanceOf[OscillatorChannel]
        oscCh.oscillator = oscillator.make(interp, oscCh.oscillatorModel)
      }
    }
  }

  object SetOscillator {
    val CODEC: MapCodec[SetOscillator] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId),
          OscillatorFactory.CODEC.fieldOf("oscillator").forGetter(_.oscillator),
        )
        .apply(inst, SetOscillator.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetOscillator] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.channelId,
      OscillatorFactory.STREAM_CODEC,
      _.oscillator,
      SetOscillator.apply _,
    )
  }

  case class SetFilter(
    channelId: ChannelId,
    filter: Option[FilterFactory],
    cutoff: ParameterValue,
    resonance: ParameterValue,
  ) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetFilter

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId); factory <- filter) {
        val filterCh = channel.asInstanceOf[FilteredChannel]
        val filter = factory.make(interp, filterCh)
        cutoff.set(interp.synth, filter.Cutoff)
        resonance.set(interp.synth, filter.Resonance)
      }
    }
  }

  object SetFilter {
    val CODEC: MapCodec[SetFilter] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId),
          FilterFactory.CODEC.fieldOf("filter").forGetter(_.filter),
          ParameterValue.CODEC.fieldOf("cutoff").forGetter(_.resonance),
          ParameterValue.CODEC.fieldOf("resonance").forGetter(_.resonance),
        )
        .apply(inst, SetFilter.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetFilter] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.channelId,
      FilterFactory.STREAM_CODEC,
      _.filter,
      ParameterValue.STREAM_CODEC,
      _.cutoff,
      ParameterValue.STREAM_CODEC,
      _.resonance,
      SetFilter.apply _,
    )
  }

  case class ResetFilter(channelId: ChannelId) extends Instruction {
    override def opcode: Opcode.Value = Opcode.ResetFilter

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        channel.asInstanceOf[FilteredChannel].filter = None
      }
    }
  }

  object ResetFilter {
    val CODEC: MapCodec[ResetFilter] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId))
        .apply(inst, ResetFilter(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, ResetFilter] = ChannelId.STREAM_CODEC.map(ResetFilter(_), _.channelId)
  }

  case class SetWaveshaper(
    channelId: ChannelId,
    shaper: ShaperFactory,
    drive: ParameterValue,
  ) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetWaveshaper

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        channel.asInstanceOf[VoiceChannel].waveshaper.shaper = shaper.make(interp)
      }
    }
  }

  object SetWaveshaper {
    val CODEC: MapCodec[SetWaveshaper] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId),
          ShaperFactory.CODEC.fieldOf("shaper").forGetter(_.shaper),
          ParameterValue.CODEC.fieldOf("drive").forGetter(_.drive),
        )
        .apply(inst, SetWaveshaper.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetWaveshaper] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.channelId,
      ShaperFactory.STREAM_CODEC,
      _.shaper,
      ParameterValue.STREAM_CODEC,
      _.drive,
      SetWaveshaper.apply _,
    )
  }

  case class ResetWaveshaper(channelId: ChannelId) extends Instruction {
    override def opcode: Opcode.Value = Opcode.ResetWaveshaper

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        channel.asInstanceOf[VoiceChannel].waveshaper.shaper = ShapingFunction.Identity
      }
    }
  }

  object ResetWaveshaper {
    val CODEC: MapCodec[ResetWaveshaper] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId))
        .apply(inst, ResetWaveshaper(_))
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, ResetWaveshaper] = {
      ChannelId.STREAM_CODEC.map(ResetWaveshaper(_), _.channelId)
    }
  }

  case class SetSync(channelId: ChannelId, depth: ParameterValue) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetSync

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        depth.set(interp.synth, channel.asInstanceOf[OscillatorChannel].Sync)
      }
    }
  }

  object SetSync {
    val CODEC: MapCodec[SetSync] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId),
          ParameterValue.CODEC.fieldOf("depth").forGetter(_.depth),
        )
        .apply(inst, SetSync.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetSync] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.channelId,
      ParameterValue.STREAM_CODEC,
      _.depth,
      SetSync.apply _,
    )
  }

  case class SetFeedback(channelId: ChannelId, gain: ParameterValue) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetFeedback

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        gain.set(interp.synth, channel.asInstanceOf[VoiceChannel].FeedbackGain)
      }
    }
  }

  object SetFeedback {
    val CODEC: MapCodec[SetFeedback] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId),
          ParameterValue.CODEC.fieldOf("gain").forGetter(_.gain),
        )
        .apply(inst, SetFeedback.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetFeedback] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.channelId,
      ParameterValue.STREAM_CODEC,
      _.gain,
      SetFeedback.apply _,
    )
  }

  case class SetOscillatorLevel(channelId: ChannelId, level: ParameterValue) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetOscillatorLevel

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        level.set(interp.synth, channel.asInstanceOf[OscillatorChannel].OscillatorLevel)
      }
    }
  }

  object SetOscillatorLevel {
    val CODEC: MapCodec[SetOscillatorLevel] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId),
          ParameterValue.CODEC.fieldOf("level").forGetter(_.level),
        )
        .apply(inst, SetOscillatorLevel.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetOscillatorLevel] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.channelId,
      ParameterValue.STREAM_CODEC,
      _.level,
      SetOscillatorLevel.apply _,
    )
  }

  case class SetChannelOutputGain(channelId: ChannelId, gain: Float) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetChannelOutputGain

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        channel.outputGain = gain
      }
    }
  }

  object SetChannelOutputGain {
    val CODEC: MapCodec[SetChannelOutputGain] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId),
          ScalaCodec.FLOAT.fieldOf("gain").forGetter(_.gain),
        )
        .apply(inst, SetChannelOutputGain.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetChannelOutputGain] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.channelId,
      ScalaStreamCodec.FLOAT,
      _.gain,
      SetChannelOutputGain.apply _,
    )
  }

  case class SetMasterSendLevel(channelId: ChannelId, level: ParameterValue) extends Instruction {
    override def opcode: Opcode.Value = Opcode.SetMasterSendLevel

    override def execute(interp: Interpreter): Unit = {
      for (channel <- interp.synth.channelById(channelId)) {
        level.set(interp.synth, channel.asInstanceOf[VoiceChannel].MasterSendLevel)
      }
    }
  }

  object SetMasterSendLevel {
    val CODEC: MapCodec[SetMasterSendLevel] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          ChannelId.CODEC.fieldOf("channel_id").forGetter(_.channelId),
          ParameterValue.CODEC.fieldOf("level").forGetter(_.level),
        )
        .apply(inst, SetMasterSendLevel.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, SetMasterSendLevel] = StreamCodec.composite(
      ChannelId.STREAM_CODEC,
      _.channelId,
      ParameterValue.STREAM_CODEC,
      _.level,
      SetMasterSendLevel.apply _,
    )
  }

  case class LoadWavetable(
    samples: ByteBuffer,
    waveSize: Int,
    oneshot: Boolean
  ) extends Instruction {
    override def opcode: Opcode.Value = Opcode.LoadWavetable

    override def execute(interp: Interpreter): Unit = {
      val floatSamples = FloatBuffer.allocate(samples.remaining())

      while (samples.hasRemaining) {
        floatSamples.put((samples.get().toInt + 128) / 255f)
      }

      val wavetable = new Wavetable(floatSamples, waveSize, oneshot)
      interp.synth.wavetable = wavetable
    }
  }

  object LoadWavetable {
    val CODEC: MapCodec[LoadWavetable] = RecordCodecBuilder.mapCodec(inst => {
      inst
        .group(
          Codec.BYTE_BUFFER.fieldOf("samples").forGetter(_.samples),
          ScalaCodec.INT.fieldOf("wave_size").forGetter(_.waveSize),
          ScalaCodec.BOOL.fieldOf("oneshot").forGetter(_.oneshot),
        )
        .apply(inst, LoadWavetable.apply _)
    })

    val STREAM_CODEC: StreamCodec[ByteBuf, LoadWavetable] = StreamCodec.composite(
      ByteBufCodecs.BYTE_ARRAY.map[ByteBuffer](ByteBuffer.wrap, _.array()),
      _.samples,
      ByteBufCodecs.VAR_INT,
      _.waveSize,
      ScalaStreamCodec.BOOL,
      _.oneshot,
      LoadWavetable.apply _,
    )
  }
}
