package ssynth.module

import scala_utils.utils.utils.Of

class Resonant_filter (name : String, enabled : Option Of Boolean = None) extends Filter (name, enabled) {
  var q = add_double_param ("Q", scala_utils.math.Range (0, 10), scala_utils.math.sqrt_2_recip)
}
