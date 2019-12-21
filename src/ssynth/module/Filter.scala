package ssynth.module

import scala_utils.utils.utils.Of

class Filter (name : String, enabled : Option Of Boolean = None) extends Module (name, enabled) {
  val freq = add_percent_param ("Freq", scala_utils.math.Range (-1.0, 1.0), 0)
  val order = add_double_param ("Order", scala_utils.math.Range (0, 16), 2)
}
