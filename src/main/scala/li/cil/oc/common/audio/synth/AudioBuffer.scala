package li.cil.oc.common.audio.synth

import li.cil.oc.Settings

/** A ring buffer of the given length (must be a power of two) that stores float samples. */
class AudioBuffer(val length: Int) {
  require((length & length - 1) == 0, "length must be a power of two")
  private val mask = length - 1
  private val samples = Array.fill(length)(0f)
  private var _wrIdx = 0
  private var _rdIdx = 0
  private var full = false

  /** Writes a sample and increments the write position. */
  @throws[IllegalStateException]("if the buffer is full")
  def put(sample: Float): Unit = {
    if (full) {
      throw new IllegalStateException("ring buffer is full")
    }

    samples(_wrIdx) = sample
    _wrIdx = _wrIdx + 1 & mask
    full = _wrIdx == _rdIdx
  }

  /** Reads a sample and increments the read position. */
  @throws[IllegalStateException]("if the buffer is empty")
  def get(): Float = {
    if (_rdIdx == _wrIdx && !full) {
      throw new IllegalArgumentException("ring buffer is empty")
    }

    val sample = samples(_rdIdx)
    _rdIdx = _rdIdx + 1 & mask
    full = false

    sample
  }

  /** Writes a value at `idx` samples ahead of the current write position. */
  def putAt(idx: Int, sample: Float): Unit = {
    samples(_wrIdx + idx & mask) = sample
  }

  /** Writes a value at `idx` samples ahead of the current read position. */
  def replace(idx: Int, sample: Float): Unit = {
    samples(_rdIdx + idx & mask) = sample
  }

  /** Reads a value at `idx` samples ahead of the current read position. */
  def getAt(idx: Int): Float = samples(_rdIdx + idx & mask)

  /** Sets the read position to the write position, effectively discarding unread samples. */
  def skipUnread(): Unit = {
    _rdIdx = _wrIdx
    full = false
  }

  /** The current write position. */
  def wrIdx: Int = _wrIdx

  /** The current read position. */
  def rdIdx: Int = _rdIdx

  /** Reads a sample at an absolute index. */
  def getAbsolute(idx: Int): Float = samples(idx & mask)

  /** Writes a sample at an absolute index. */
  def putAbsolute(idx: Int, sample: Float): Unit = {
    samples(idx & mask) = sample
  }

  def wrapIndex(idx: Int): Int = idx & mask

  /** Increments the write position by the given offset. */
  @throws[IllegalArgumentException]("if the buffer does not have enough space")
  def bumpWritePos(offset: Int): Unit = {
    if (offset < 0 || offset >= length - remaining) {
      throw new IllegalArgumentException("not enough space in ring buffer")
    }

    _wrIdx = _wrIdx + offset & mask
  }

  def remaining: Int = {
    if (full) {
      0
    } else {
      _wrIdx - _rdIdx & mask
    }
  }

  def isEmpty: Boolean = _wrIdx == _rdIdx && !full

  def nonEmpty: Boolean = !isEmpty
}

object AudioBuffer {
  def makeBlockSized(): AudioBuffer = new AudioBuffer(Settings.get.audioBlockSize)
}
