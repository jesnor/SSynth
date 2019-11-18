package ssynth

import ssynth.util.utils.Array_float

import scala.collection.mutable.ArrayBuffer

class Synth (samples_fn : Float => Array_float, val fade_time : Float = 0.001f) {
  class Voice (val freq : Float) {
    val synth = Synth.this
    var time = 0.0
    var amp = 0.0
    var amp_k = 0.0
    private var _samples : Array_float = _

    def samples = {
      if (_samples == null)
        _samples = samples_fn (freq)

      _samples
    }

    def fade_out () = amp_k = -fade_time
  }

  val voices = new ArrayBuffer [Voice]

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

        if (voice.amp_k != 0) {
          voice.amp += time_scale / voice.amp_k

          if (voice.amp >= 1) {
            voice.amp = 1
            voice.amp_k = 0
          }
          else if (voice.amp <= 0) {
            voice.amp = 0
            voice.amp_k = 0
          }
        }
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

  def play_tone (freq : Float) : Voice = new Voice (freq) {
    amp_k = fade_time
    voices += this
  }

  private def drop_completed_voices () =
    voices.filterInPlace (v => v.amp != 0)

  def update_voices () : Unit = {
    drop_completed_voices ()

    voices.addAll (voices.map (v => {
      v.fade_out ()

      new Voice (v.freq) {
        amp_k = fade_time
        time = v.time
      }
    }))
  }
}
