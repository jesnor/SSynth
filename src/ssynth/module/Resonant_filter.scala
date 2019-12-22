package ssynth.module

class Resonant_filter (name : String) extends Filter (name) {
  var q = add_param ("Q", scala_utils.math.sqrt_2_recip, scala_utils.math.Range (0, 10.0))
}
