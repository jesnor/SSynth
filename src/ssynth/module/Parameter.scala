package ssynth.module

import scala_utils.math.{Range_double, Range_int}

trait Parameter [T] {
  val name : String
  val default : T
  def apply () : T
  def update (v : T) : Unit
  def value : T = this ()
  def value_string (v : T) : String = v.toString
}

sealed abstract class CParameter [T] (val module : Module, val name : String, val default : T) extends Parameter [T] {
  type Value = T

  private var _value : T = default

  def update (v : T) : Unit = _value = v
  def apply () : T = _value
}

class Int_parameter (module : Module, name : String, val range : Range_int, default : Int = 0)
    extends CParameter (module, name, default) {
  override def value_string (v : Int) = v.toString
}

class Double_parameter (module : Module,
                        name : String,
                        val range : Range_double,
                        default : Double = 0) extends CParameter (module, name, default) {
  override def value_string (v : Double) = v.round.toString
}

class Enum_parameter [T] (module : Module, name : String, val values : Seq [T], default : T)
    extends CParameter (module, name, default) {
  def this (module : Module, name : String, values : Seq [T]) = this (module, name, values, values.head)
}

class Boolean_parameter (module : Module, name : String, default : Boolean = false)
    extends Enum_parameter [Boolean](module, name, Seq (false, true), default)
