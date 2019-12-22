package ssynth.module

import scala_utils.parameter.Parameter_group

class Filter (name : String) extends Parameter_group (name) {
  val freq = add_percent_param ("Freq", 0, scala_utils.math.Range (-1.0, 1.0))
  val order = add_param ("Order", 2.0, scala_utils.math.Range (0, 16.0))
}
