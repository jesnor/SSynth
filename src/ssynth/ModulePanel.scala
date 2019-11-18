package ssynth

import java.awt.{Color, Dimension, Font}
import javax.swing.BorderFactory

import scala.swing.GridBagPanel.{Anchor, Fill}
import scala.swing.event.{ButtonClicked, SelectionChanged, ValueChanged}
import scala.swing.{Alignment, CheckBox, ComboBox, GridBagPanel, Insets, Label, Slider}

class ModulePanel (title: String) extends GridBagPanel {
  border = BorderFactory.createLineBorder (Color.black)
  val c = new Constraints
  c.insets = new Insets (4, 4, 4, 4)

  val title_label = new Label (title)
  title_label.font = title_label.font.deriveFont (Font.BOLD, title_label.font.getSize + 2)

  c.gridx = 0
  c.gridy = 0
  c.gridwidth = 0
  c.anchor = Anchor.NorthWest
  c.weightx = 0
  c.weighty = 0
  layout (title_label) = c

  c.gridwidth = 1


/*  def create_combo_box [T] (p : Parameter [T], values : Seq [Any]) = new ComboBox (values) {
    reactions += {
      case SelectionChanged (_) => {
        p (selection.item.asInstanceOf [T])
      }
    }

    listenTo (selection)
    val conn = p.now_and_changed (v => selection.item = v)
  }

  for ((p, i) <- module.parameters.zipWithIndex) {
    c.gridy = i + 1

    c.gridx = 0
    c.anchor = Anchor.West
    c.fill = Fill.None
    layout (new Label (p.name)) = c

    val editors = p match {
      case b : BooleanParameter => Seq (new CheckBox () {
        selected = b ()

        reactions += {
          case ButtonClicked (_) => b (selected)
        }

        val conn = b.now_and_changed (v => {
          selected = b ()
        })
      })

      case n : NumberParameter => {
        val label = new Label
        label.preferredSize = new Dimension(70, 0)
        label.horizontalAlignment = Alignment.Right

        val slider = new Slider {
          min = if (n.int) n.min.toInt else 0
          max = if (n.int) n.max.toInt else 100
          var disable_reaction = false

          reactions += {
            case ValueChanged (_) if !disable_reaction => {
              val x = value / max.toDouble
              val px = math.pow(x, n.exp)
              n (if (n.int) value else (px * (n.max - n.min) + n.min))
            }
          }

          listenTo (this)

          val conn = n.now_and_changed (v => {
            val nv = (v - n.min) / (n.max - n.min)
            val x = math.pow(nv, 1 / n.exp)
            disable_reaction = true
            value = (if (n.int) v else x * max).toInt
            label.text = if (n.int) v.toInt.toString else v.formatted("%1$.2f")
            disable_reaction = false
          })
        }

        Seq (slider, label)
      }

      case e : EnumParameter [_] => Seq (create_combo_box (e, e.values))
      case _ => Seq (create_combo_box (p, get_values (p)))
    }

    c.gridx = 1
    c.fill = Fill.Both
    layout (editors (0)) = c

    if (editors.size > 1) {
      c.gridx = 2
      c.anchor = Anchor.East
      layout (editors (1)) = c
    }
  }*/
}
