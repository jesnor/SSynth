package ssynth

import org.jtransforms.fft.FloatFFT_1D
import scala_utils.math
import scala_utils.utils.observables.CObservable
import scala_utils.utils.Array_float

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Synth (model : Synth_params, max_freq : Double, val fade_time : Float = 0.001f) extends CObservable [Synth] {
  val parameter_observer = model.add_observer (_ => {
    samples_cache.clear ()
    update_voices ()
    fireChange (Synth.this)
  })

  val max_voice_count = 1
  val max_harmonics_count_shift = 9
  val max_harmonics_count = 1 << max_harmonics_count_shift
  val frame_size = 1 << (max_harmonics_count_shift + 2)
  val fft = new FloatFFT_1D (frame_size)
  var spectrum_time = 0L
  var fft_time = 0L
  val samples_cache = new mutable.HashMap [Int, (Array_float, Array_float)]

  def harmonic_count (freq : Double) = (max_freq / freq).toInt

  def to_harmonic_freq (f : Double) = scala.math.pow (2, max_harmonics_count_shift * f)
  def to_normalized_freq (f : Double) = scala.math.log (f) / math.log2 / max_harmonics_count_shift

  /**
   * Compensation for equal-loudness
   *
   * From 500 Hz is perceived as approximately 20-40 times louder than 40 Hz at 60 - 80 dB SPL
   *
   * @param base_freq
   * @param freq
   * @return
   */
  def bass_boost (base_freq: Double, freq: Double) =
     1.0 / (1 + model.harmonics.bass_boost() * 10 * (freq.max(40).min(500) - 40) / (500 - 40))

  def filter_gain (freq : Double) = {
    if (model.filter_control.enabled ()) {
      val f0 = model.filter_control.freq ()

      val lf =
        if (model.butterworth_lp_filter.enabled ())
          utils.state_variable_lpf_gain (freq / to_harmonic_freq (f0 + model.butterworth_lp_filter.freq ()), model.butterworth_lp_filter.q (),
            model.butterworth_lp_filter.order ())
        else 1.0

      val sf =
        if (model.sinc_lp_filter.enabled ())
          utils.sinc_filter_gain (Math.min (1, (freq - 1) / to_harmonic_freq (f0 + model.sinc_lp_filter.freq ())), model.sinc_lp_filter.order ())
        else 1.0

      val rf = if (model.notch_filter.enabled ())
        utils.calc_resonator_gain (
          freq / to_harmonic_freq (f0 + model.notch_filter.freq ()),
          model.notch_filter.gain (),
          scala.math.pow (model.notch_filter.width (), 3))
      else 1.0

      lf * sf * rf
    }
    else 1.0
  }

  def get_samples (freq : Double) = {
    val hc = harmonic_count (freq)
        .min (model.harmonics.count ())
        .min ((model.harmonics.max_freq () / freq).toInt.max (1))

    samples_cache.getOrElse (hc, generate_samples (freq, hc))
  }

  def generate_samples (freq: Double, hc : Int) : (Array_float, Array_float) = {
    val spectrum = new Array_float (frame_size)

    def is_oct_div (f : Int, d : Int) : Boolean = {
      for (i <- 0 until 31) {
        if ((1 << i) > f) {
          for (k <- 0 until d * 2)
            if (f * d == (1 << (i - 1)) * k)
              return true

          return false
        }
      }

      false
    }

    spectrum_time = utils.measure_nanos {
      /*      for {
              k <- 1 to 20
              count = hc / k
              i <- 1 to count
              f = i * k
            } {
              val saw_a = 0
              val saw_p = 2 / Math.PI * Math.sin (Math.PI * (i * (0.5 - pulse_width) - 0.5))
              val a1 = saw_a * Math.pow (i, -osc_slope)
              val a2 = saw_p * Math.pow (i, -osc_slope)
              val gc = Math.pow (math.sinc_normalized (i.toDouble / (count + 1)), osc_sinc_exp)
              val c = 1.0 // Math.pow (Math.abs (Math.cos (osc_cos_freq * i * 2 * Math.PI)), osc_cos_exp)
              val gain = c * gc / Math.pow (k, 1.1) * filter_gain (f)

              s (f * 2) += (gain * a1).toFloat
              s (f * 2 + 1) += (gain * a2).toFloat
            }*/
      for {
        i <- 1 to hc

        if i == 1 ||
            model.harmonics.divider () == 0 ||
            is_oct_div (i, model.harmonics.divider ())
      } {
        val saw_a = 0
        val saw_p = 2 / Math.PI * Math.sin (Math.PI * (i * (0.5 - model.harmonics.pulse_width ()) - 0.5))
        val square_a = 4 / Math.PI * Math.sin (Math.PI * i * model.harmonics.pulse_width ())
        val square_p = 0.0
        val a1 = math.interpolate (model.harmonics.saw_square (), saw_a, square_a) * Math.pow (i, -model.harmonics.slope ())
        val a2 = math.interpolate (model.harmonics.saw_square (), saw_p, square_p) * Math.pow (i, -model.harmonics.slope ())
        val gc = Math.pow (math.sinc_normalized (i.toDouble / (hc + 1)), model.harmonics.sinc_exp ())
        val c = Math.pow (Math.abs (Math.cos (model.harmonics.cos_freq () * i * 2 * Math.PI)), model.harmonics.cos_exp ())
        val fg = filter_gain(i)
        val bb = bass_boost(freq, freq * i)
        val gain = c * gc * fg * bb

        spectrum (i * 2) = (gain * a1).toFloat
        spectrum (i * 2 + 1) = (gain * a2).toFloat
      }
    }

    val samples = spectrum.clone ()
    fft_time = utils.measure_nanos (fft.realInverse (samples, false))
    val max = Math.max (-samples.min, samples.max)
    var gain = model.amp.gain () / max_voice_count

    if (gain * max > 2.0)
      gain = 2.0 / max

    for (i <- samples.indices)
      samples (i) *= gain.toFloat

    samples_cache (hc) = (spectrum, samples)
    (spectrum, samples)
  }

  val notes_playing = new mutable.HashMap [Int, Seq [Synth#Voice]]

  def play_note (n : Int) = if (!notes_playing.contains (n)) {
    val nf = note_to_freq (n)

    val unison_voices =
      if (model.unison.enabled ())
        for {
          i <- 0 until model.unison.count () * 2
          dn = 2.0 * i / (model.unison.count () * 2 - 1) - 1
        } yield play_tone (
          nf * (1 + model.unison.spread () * dn),
          model.unison.gain () * (1 - Math.abs (dn) * model.unison.slope ()),
          (i + 1) / (model.unison.count () * 2 + 1))
      else Seq ()

    notes_playing (n) = play_tone (nf, 1) +: unison_voices
  }

  def stop_playing_note (n : Int) : Boolean = notes_playing.get (n).exists {
    voices =>
      for (v <- voices)
        v.fade_out ()

      notes_playing.remove (n)
      true
  }

  def note_to_freq (n : Double) = 440.0 * Math.pow (2, (n - 69) / 12.0)

  class Voice (val freq : Double, var amp : Double) {
    val synth = Synth.this
    var time = 0.0
    var fade_amp = 0.0
    var amp_k = 0.0
    var _samples : Array_float = _

    def samples = {
      if (_samples == null)
        _samples = get_samples (freq)._2

      _samples
    }

    def fade_out () = {
      amp_k = -fade_time
    }

    def fade_in () = amp_k = fade_time

    def copy () = {
      val v = new Voice (freq, amp)
      v.time = time
      v.fade_amp = fade_amp
      v.amp_k = amp_k
      v._samples = _samples
      v
    }
  }

  val voices = new ArrayBuffer [Voice]

  def mix_voices (sample_rate : Float, out : Array_float, offset : Int, length : Int) : Unit = {
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

        val s = (1 - frac) * voice.samples (t1 & (voice.samples.length - 1)) +
            frac * voice.samples ((t1 + 1) & (voice.samples.length - 1))

        sum += voice.amp * voice.fade_amp * s

        if (voice.amp_k != 0)
          voice.fade_amp = 0.0.max (1.0.min (voice.fade_amp + time_scale / voice.amp_k))
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

  def play_tone (freq : Double, amp : Double, time_offset : Double = 0) : Voice = new Voice (freq, amp) {
    time = time_offset
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
        v.fade_amp = 0
        v._samples = null
        Seq (v2)
      }
      else
        Seq.empty
    }))
  }

  private def drop_completed_voices () =
    voices.filterInPlace (v => v.fade_amp > 0 || v.amp_k >= 0)
}
