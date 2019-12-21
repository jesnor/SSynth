package ssynth.module

import scala_utils.math.{Range, Range_double, Range_int}
import scala_utils.utils.utils.Of

import scala.collection.mutable.ArrayBuffer

class Module (val name : String, _enabled : Option Of Boolean = None) {
  var parent : Option [Module] = None
  val modules = new ArrayBuffer [Module]()
  val parameters = new ArrayBuffer [Parameter [_]]()
  var enabled_param : Option [Parameter [Boolean]] = _enabled.map (s => add_boolean_param ("Enabled", s))

  def enabled = enabled_param.forall (_ ())

  def add_percent_param (name : String) : Parameter Of Double =
    add_percent_param (name, Range (0.0, 1.0), 0)

  def add_percent_param (name : String, default : Double) : Parameter Of Double =
    add_percent_param (name, Range (0.0, 1.0), default)

  def add_percent_param (name : String, range : Range_double, default : Double = 0) : Parameter Of Double =
    add_param (new Double_parameter (this, name, range, range.clamp (default)) {
      override def value_string (v : Double) = (v * 100).round + "%"
    })

  def add_double_param (name : String, range : Range_double, default : Double = 0) =
    add_param (new Double_parameter (this, name, range, default))

  def add_int_param (name : String, range : Range_int, default : Int = 0) =
    add_param (new Int_parameter (this, name, range, default))

  def add_boolean_param (name : String, default : Boolean = false) =
    add_param (new Boolean_parameter (this, name, default))

  def add_param [T] (p : Parameter [T]) : Parameter [T] = {
    parameters += p
    p
  }

  def add_module (m : Module) = {
    m.parent = Some (this)
    modules += m
    m
  }

  def param_changed (p : Parameter [_]) : Unit = parent.foreach (_.param_changed (p))
}
