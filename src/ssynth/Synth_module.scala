package ssynth

import scala_utils.math.Range
import ssynth.module.{Filter, Module, Resonant_filter}

class Synth_module (max_freq : Double) extends Module ("FFT Synth") {
  object amp extends Module ("Amp") {
    val gain = add_percent_param ("Gain", 0.5)
    val clipping = add_boolean_param ("Clipping")
  }

  object harmonics extends Module ("Harmonics") {
    val saw_square = add_percent_param ("Saw/square")
    val pulse_width = add_percent_param ("PW", 0.5)
    val count = add_int_param ("Count", Range (1, 512), 512)
    val max_freq = add_double_param ("Max freq", Range (0, Synth_module.this.max_freq), Synth_module.this.max_freq)
    val divider = add_int_param ("Divider", Range (0, 100))
    val slope = add_percent_param ("Slope", Range (0, 2), 1)
    val sinc_exp = add_double_param ("Gibbs exp", Range (0, 3), 1.5)
    val cos_freq = add_percent_param ("Cos freq")
    val cos_exp = add_double_param ("Cos exp", Range (0, 10), 1)
  }

  object unison extends Module ("Unison", Some (false)) {
    var gain = add_percent_param ("Gain", 0.5)
    var slope = add_percent_param ("Slope", 0.3)
    var count = add_int_param ("Count", Range (1, 16), 2)
    var spread = add_percent_param ("Spread", 0.01)
  }

  object filter_control extends Module ("Filter", Some (true)) {
    val freq = add_percent_param ("Freq", 1)
  }

  object butterworth_lp_filter extends Module ("Butterworth LP", Some (false)) {
    val freq = add_percent_param ("Freq", Range (-1.0, 1.0))
    val order = add_double_param ("Order", scala_utils.math.Range (0, 16), 2)
    var q = add_double_param ("Q", scala_utils.math.Range (0, 10), scala_utils.math.sqrt_2_recip)
  }

  object sinc_lp_filter extends Module ("Sinc LP", Some (false)) {
    val freq = add_percent_param ("Freq", Range (-1.0, 1.0))
    val order = add_double_param ("Order", scala_utils.math.Range (0, 16), 2)
  }

  object notch_filter extends Module ("Notch", Some (false)) {
    val freq = add_percent_param ("Freq", Range (-1.0, 1.0))
    val gain = add_double_param ("Gain", Range (0.0, 10.0), 1)
    val width = add_percent_param ("Width", Range (0.0, 1.0), 0.3)
  }

  add_module (amp)
  add_module (harmonics)
  add_module (unison)
  add_module (filter_control)
  add_module (butterworth_lp_filter)
  add_module (sinc_lp_filter)
  add_module (notch_filter)
}
