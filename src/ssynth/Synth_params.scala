package ssynth

import scala_utils.math.Range
import scala_utils.parameter.Parameter_group

class Synth_params (max_freq : Double) extends Parameter_group ("FFT Synth") {
  object amp extends Parameter_group ("Amp") {
    val gain = add_percent_param ("Gain", 1)
    val clipping = add_boolean_param ("Clipping")
  }

  object harmonics extends Parameter_group ("Harmonics") {
    val saw_square = add_percent_param ("Saw/square", 0)
    val pulse_width = add_percent_param ("PW", 0.5)
    val count = add_param ("Count", 512, Range (1, 512))
    val max_freq = add_param ("Max freq", Synth_params.this.max_freq, Range (0, Synth_params.this.max_freq))
    val divider = add_param ("Divider", 0, Range (0, 100))
    val slope = add_percent_param ("Slope", 1, Range (0, 2.0))
    val sinc_exp = add_param ("Gibbs exp", 1.5, Range (0, 3.0))
    val cos_freq = add_percent_param ("Cos freq", 0)
    val cos_exp = add_param ("Cos exp", 1.0, Range (0.0, 10.0))
  }

  object unison extends Parameter_group ("Unison") {
    val enabled = add_boolean_param("Enabled")
    val gain = add_percent_param ("Gain", 0.5)
    val slope = add_percent_param ("Slope", 0.3)
    val count = add_param ("Count", 2, Range (1, 16))
    val spread = add_percent_param ("Spread", 0.01)
  }

  object filter_control extends Parameter_group ("Filter") {
    val enabled = add_boolean_param("Enabled", true)
    val freq = add_percent_param ("Freq", 1)
  }

  object butterworth_lp_filter extends Parameter_group ("Butterworth LP") {
    val enabled = add_boolean_param("Enabled", true)
    val freq = add_percent_param ("Freq", 0, Range (-1.0, 1.0))
    val order = add_param ("Order", 2.0, scala_utils.math.Range (0, 16.0))
    var q = add_param ("Q", scala_utils.math.sqrt_2_recip, scala_utils.math.Range (0, 10.0))
  }

  object sinc_lp_filter extends Parameter_group ("Sinc LP") {
    val enabled = add_boolean_param("Enabled")
    val freq = add_percent_param ("Freq", 0, Range (-1.0, 1.0))
    val order = add_param ("Order", 2.0, scala_utils.math.Range (0, 16.0))
  }

  object notch_filter extends Parameter_group ("Notch") {
    val enabled = add_boolean_param("Enabled")
    val freq = add_percent_param ("Freq", 0, Range (-1.0, 1.0))
    val gain = add_param ("Gain", 1.0, Range (0.0, 10.0))
    val width = add_percent_param ("Width", 0.3, Range (0.0, 1.0))
  }

  add_group (amp)
  add_group (harmonics)
  add_group (unison)
  add_group (filter_control)
  add_group (butterworth_lp_filter)
  add_group (sinc_lp_filter)
  add_group (notch_filter)
}
