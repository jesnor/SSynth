package ssynth

import scala_utils.math.{Range, Set}
import scala_utils.parameter.{Parameter, Parameter_group}
import scala_utils.swing.Knob_with_labels
import scala_utils.swing.utils.{check_box, flow_panel_s, label, titled_panel, vbox}

import scala.swing.event.ButtonClicked
import scala.swing.{CheckBox, Component, Panel}

package object swing {
  def enabled_parameter (group : Parameter_group) =
    group.parameters.find (_.name == "Enabled").collect (p => p.default match {
      case _ : Boolean => p.asInstanceOf [Parameter [Boolean]]
    })

  def parameter_editor [T] (p : Parameter [T]) : Option [Component] =
    (p, p (), p.valid_values) match {
      case (ip : Parameter [Int] @unchecked, i : Int, r : Range [Int]) => Some (
        new Knob_with_labels (p.name, r.to_double, i, v => {
          ip () = v.round.toInt
        }, 1, v => ip.value_string (v.round.toInt)) {
          val parameter_observer = ip.add_observer (vc => set_value (vc.new_value))
        })

      case (dp : Parameter [Double] @unchecked, d : Double, r : Range [Double]) => Some (
        new Knob_with_labels (dp.name, r, dp (), v => {
          dp () = v
        }, 1, dp.value_string) {
          val parameter_observer = dp.add_observer (vc => set_value (vc.new_value))
        })

      case (bp : Parameter [Boolean] @unchecked, _, Set.booleans) =>
        val cb = new CheckBox (bp.name) {
          val parameter_observer = bp.now_and_on_change {
            selected = bp ()
          }

          reactions += {
            case ButtonClicked (_) => bp () = selected
          }
        }

        Some (cb)

      case _ => None
    }

  def parameter_group_panel (group : Parameter_group) : Panel = {
    val parameter_editors = group.parameters.filter (_.name != "Enabled").flatMap (parameter_editor (_))
    val module_panels = group.groups map parameter_group_titled_panel
    vbox (flow_panel_s (parameter_editors), flow_panel_s (module_panels))
  }

  def parameter_group_titled_panel (group : Parameter_group) : Panel = {
    val title_component = enabled_parameter (group).map (p => check_box (group.name, p (), v => {
      p () = v
    })).getOrElse (label (group.name))

    titled_panel (title_component, parameter_group_panel (group))
  }
}
