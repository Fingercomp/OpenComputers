package li.cil.oc.common.audio.synth.channel

import li.cil.oc.common.audio.synth.filter.PerSampleFilter

trait FilteredChannel extends Channel {
  private var _filter: Option[PerSampleFilter] = None

  def filter: Option[PerSampleFilter] = _filter

  def filter_=(filter: Option[PerSampleFilter]): Unit = {
    (_filter, filter) match {
      case (None, None) =>
      case (Some(prev), Some(next)) if prev eq next =>
      case _ =>
        _filter.foreach(_.reset())
        _filter = filter
    }
  }

  override def reset(): Unit = {
    super.reset()

    filter = None
  }
}
