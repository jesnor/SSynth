package ssynth

import scala_utils.swing.utils.{check_box, flow_panel_s, knob_with_labels, label, titled_panel, vbox}
import ssynth.module._

import scala.swing.{Component, Panel}

package object swing {
  def parameter_editor [T] (p : Parameter [T], on_change : Parameter [_] => Unit) : Component = p match {
    case ip : Int_parameter => knob_with_labels (ip.name, ip.range.to_double, ip (), v => {
      ip () = v.round.toInt
      on_change (p)
    }, 1, v => ip.value_string (v.round.toInt))

    case dp : Double_parameter => knob_with_labels (dp.name, dp.range, dp (), v => {
      dp () = v
      on_change (p)
    }, 1, dp.value_string)

    case bp : Boolean_parameter => check_box (bp.name, bp (), v => {
      bp () = v
      on_change (p)
    })

    case ep : Enum_parameter [_] => null // TODO:
  }

  def module_panel (m : Module, on_change : Parameter [_] => Unit) : Panel = {
    val parameter_editors = m.parameters.filterNot (m.enabled_param.contains (_)).map (parameter_editor (_, on_change))
    val module_panels = m.modules map (module_titled_panel (_, on_change))
    vbox (flow_panel_s (parameter_editors), flow_panel_s (module_panels))
  }

  def module_titled_panel (m : Module, on_change : Parameter [_] => Unit) : Panel = {
    val title_component = m.enabled_param.map (p => check_box (m.name, p.value, v => {
      p () = v
      on_change (p)
    })).getOrElse (label (m.name))

    titled_panel (title_component, module_panel (m, on_change))
  }
}
