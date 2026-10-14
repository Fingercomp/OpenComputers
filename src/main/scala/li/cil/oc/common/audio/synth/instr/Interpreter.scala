package li.cil.oc.common.audio.synth.instr

import li.cil.oc.Settings
import li.cil.oc.common.audio.synth.{Resettable, Synth}
import org.lwjgl.BufferUtils

import java.nio.ByteBuffer
import scala.collection.mutable

class Interpreter(voiceChannelCount: Int, lfoChannelCount: Int) extends Resettable {
  val synth: Synth = new Synth(voiceChannelCount, lfoChannelCount)
  val instrs: mutable.ArrayDeque[Instruction] = mutable.ArrayDeque.empty

  private var delayRemainingSec = 0f

  override def reset(): Unit = {
    super.reset()
    synth.reset()
    instrs.clear()
    delayRemainingSec = 0f
  }

  def interpret(samples: Int): Option[ByteBuffer] = {
    val output = BufferUtils.createByteBuffer(samples)

    while (true) {
      // if there's at least one sample of unprocessed delay, go for it!
      if (delayRemainingSec > Settings.get.audioSamplePeriodSec) {
        // but don't overflow the buffer in doing so.
        val samples = (delayRemainingSec * Settings.get.audioCardSampleRate).toInt min output.remaining
        synth.process(samples, output)
        delayRemainingSec -= samples * Settings.get.audioSamplePeriodSec

        if (!output.hasRemaining) {
          // leave the rest of the delay (if any) for the next interpret call.
          return Some(output)
        }
      }

      if (instrs.isEmpty) {
        // there are no more instructions to run and no delay pending, so finish with what we're got.
        output.flip()

        return Option.when(output.hasRemaining) { output }
      }

      instrs.removeHead().execute(this)
    }

    // unreachable.
    None
  }

  private[instr] def delay(seconds: Float): Unit = {
    delayRemainingSec += seconds
  }
}
