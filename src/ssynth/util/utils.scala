package ssynth.util

object utils {
  type Of [A [_], B] = A [B]
  type Array_float = Array [Float]

  val log2 = math.log (2)
  val log10_2 = math.log10 (2)

  def db_to_amp (db : Double) = math.sqrt (math.pow (10, db / 10))
}
