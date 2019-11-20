package ssynth

import ssynth.util.utils.Array_float

import scala.collection.mutable.ArrayBuffer

class Synth (samples_fn : Double => Array_float, val fade_time : Float = 0.01f) {
  def generate_samples (sample_rate : Float, out : Array_float, offset : Int, length : Int) : Unit = {
    val time_scale = 1f / sample_rate

    for (i <- 0 until length) {
      val frame_pos = i * time_scale
      var sum = 0.0

      for {
        voice <- voices
      } {
        val t = (voice.time + frame_pos * voice.freq) * voice.samples.length
        val t1 = t.toInt
        val frac = t - t1
        val s = (1 - frac) * voice.samples (t1 & (voice.samples.length - 1)) + frac * voice.samples ((t1 + 1) & (voice.samples.length - 1))
        sum += voice.amp * s

        if (voice.amp_k != 0)
          voice.amp = 0.0.max (1.0.min (voice.amp + time_scale / voice.amp_k))
      }

      out (offset + i) = sum.toFloat
    }

    drop_completed_voices ()

    // Update voice times
    for (v <- voices) {
      val t = v.time + v.freq * length * time_scale
      v.time = t - t.floor
    }
  }

  val voices = new ArrayBuffer [Voice]
  def play_tone (freq : Double) : Voice = new Voice (freq) {
    amp_k = fade_time
    voices += this
  }
  def update_voices () : Unit = {
    drop_completed_voices ()

    voices.addAll (voices.flatMap (v => {
      if (v.amp_k >= 0 && v._samples != null) {
        val v2 = v.copy ()
        v2.fade_out ()
        v.fade_in ()
        v.amp = 0
        v._samples = null
        Seq (v2)
      }
      else
        Seq.empty
    }))
  }
  private def drop_completed_voices () =
    voices.filterInPlace (v => v.amp > 0 || v.amp_k >= 0)
  class Voice (val freq : Double) {
    val synth = Synth.this
    var time = 0.0
    var amp = 0.0
    var amp_k = 0.0
    var _samples : Array_float = _

    def samples = {
      if (_samples == null)
        _samples = samples_fn (freq)

      _samples
    }

    def fade_out () = {
      amp_k = -fade_time
    }

    def fade_in () = amp_k = fade_time

    def copy () = {
      val v = new Voice (freq)
      v.time = time
      v.amp = amp
      v.amp_k = amp_k
      v._samples = _samples
      v
    }
  }
}
