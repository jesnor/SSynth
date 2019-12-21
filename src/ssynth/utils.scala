package ssynth

import scala_utils.math

object utils {
  def measure_nanos (f : => Unit) = {
    val t = System.nanoTime ()
    f
    System.nanoTime () - t
  }

  def butterworth_lpf_gain (f : Double, q : Double, order : Double) = {
    val f2 = f * f
    val c = 1 / Math.max (q, 0.000001)
    1 / Math.pow (f2 * f2 + f2 * (c * c - 2) + 1, order / 4)
  }

  def state_variable_lpf_gain (f : Double, q : Double, order : Double) = {
    butterworth_lpf_gain (f, q, order)
  }

  def calc_resonator_gain (freq : Double, gain : Double, width : Double) = {
    val r = Math.log (freq)
    val r2 = r * r
    val s = gain * ((width - r2) / (r2 + width) + 1) + 1
    s
  }

  def calc_sinc_filter_cutoff (order : Double) = math.sinc_normalized_inverse (Math.pow (1.0 / Math.sqrt (2), 1 / order))

  // Probably possible to invert this function but couldn't bother
  def calc_butterworth_cutoff (q : Double, order : Double) =
    math.inverse_monotonic (1, -0.5, math.sqrt_2_recip, butterworth_lpf_gain (_, q, order))

  def sinc_filter_gain (freq : Double, order : Double) = Math.pow (math.sinc_normalized (freq), order)
}
