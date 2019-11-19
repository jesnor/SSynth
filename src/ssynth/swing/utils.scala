package ssynth.swing

import java.awt.{Color, Toolkit}

import javax.swing.UIManager
import javax.swing.border.{CompoundBorder, EmptyBorder, LineBorder, MatteBorder}
import ssynth.math.Point_2i

import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.swing.{Action, Alignment, BorderPanel, BoxPanel, Button, CheckBox, Component, Dimension, FlowPanel, Graphics2D, GridBagPanel, Label, MainFrame, Orientation, Point, Slider, ToggleButton}

object utils {
  val title_border = new CompoundBorder (new MatteBorder (0, 0, 1, 0, Color.BLACK), new EmptyBorder (2, 4, 2, 4))

  def label (text : String) = new Label (text)

  def set_bold (c : Component) =
    c.font = c.font.deriveFont (java.awt.Font.BOLD, c.font.getSize)

  def title_label (text : String) = new Label (text) {
    set_bold (this)
  }

  def button (text : String, action : () => Unit) = new Button (new Action (text) {
    override def apply () : Unit = action ()
  })

  def slider (min : Int, max : Int, value : Int, action : Int => Unit) = {
    val s = new Slider
    s.min = min
    s.max = max
    s.value = value

    s.reactions += {
      case ValueChanged (_) =>
        action (s.value)
    }

    s listenTo s
    s
  }

  def toggle_button (text : String, s : Boolean, a : Boolean => Unit) = new ToggleButton (text) {
    selected = s

    reactions += {
      case ButtonClicked (_) => a (selected)
    }
  }

  def check_box (text : String, s : Boolean, a : Boolean => Unit) = new CheckBox (text) {
    reactions += {
      case ButtonClicked (_) => a (selected)
    }

    selected = s
  }

  def box (o : Orientation.Value, children : Component*) = new BoxPanel (o) {
    contents.appendAll (children)
  }

  def vbox (children : Component*) = box (Orientation.Vertical, children : _*)
  def hbox (children : Component*) = box (Orientation.Horizontal, children : _*)
  def vboxs (children : Seq [Component]) = box (Orientation.Vertical, children : _*)
  def hboxs (children : Seq [Component]) = box (Orientation.Horizontal, children : _*)

  def flow_panel (children : Component*) = new FlowPanel {
    contents.appendAll (children)
  }

  def main_frame (name : String, s : Dimension, child : Component) = new MainFrame () {
    title = name
    contents = child

    size = s
    val dimension = Toolkit.getDefaultToolkit.getScreenSize
    location = new Point (((dimension.getWidth - s.width) / 2).toInt, ((dimension.getHeight - s.height) / 2).toInt)
  }

  def top_center_panel (top : Component, center : Component) = new BorderPanel {
    add (top, BorderPanel.Position.North)
    add (center, BorderPanel.Position.Center)
  }

  def titled_panel (top : Component, center : Component) : Component = {
    set_bold (top)

    val p = top_center_panel (new BorderPanel {
      add (top, BorderPanel.Position.West)
      border = title_border
    },
      center)

    p.border = new LineBorder (Color.BLACK, 2)
    p
  }

  def titled_panel (title : String, center : Component) : Component = titled_panel (label (title), center)

  case class Dimension_ops (d : Dimension) extends AnyVal {
    def to_point = Point_2i (d.width, d.height)
  }

  implicit def dimension_ops (d : Dimension) : Dimension_ops = Dimension_ops (d)

  def set_look_and_feel (name : String) : Unit =
    for (info <- UIManager.getInstalledLookAndFeels) {
      if (name == info.getName)
        UIManager.setLookAndFeel (info.getClassName)
    }

  def set_nimbus_look_and_feel () : Unit = set_look_and_feel ("Nimbus")

  def slider_with_label (min : Int, max : Int, value : Int, action : Int => Unit) = {
    val l = label (value.toString)
    l.preferredSize = new Dimension (70, 0)
    l.horizontalAlignment = Alignment.Right

    val s = slider (min, max, value, v => {
      l.text = v.toString
      action (v)
    })

    Seq (s, l)
  }

  def knob_with_labels (title : String,
                        min : Double,
                        max : Double,
                        value : Double,
                        action : Double => Unit,
                        exp : Double = 1,
                        to_string : Double => String = _.round.toString) = {
    val title_label = label (title)
    val value_label = label (to_string (value))

    val knob = new Knob (min, max, value, v => {
      value_label.text = to_string (v)
      action (v)
    }, exp)

    val p = new GridBagPanel {
      val c = new Constraints
      add (title_label, c)
      c.gridy = 1
      add (knob, c)
      c.gridy = 2
      add (value_label, c)
    }

    /*    p.layout (title_label).grid = (0, 0)
        p.layout (value_label).grid = (1, 0)
        p.layout (knob).grid = (1, 0)
        p.layout (knob).gridwidth = 2
      */
    p
  }

  def draw_string (g : Graphics2D, text : String, x : Double, y : Double, anchor_x : Double = 0, anchor_y : Double = 0) = {
    val b = g.getFontMetrics.getStringBounds (text, g)
    g.drawString (text, (x - anchor_x * b.getWidth).toInt, (y + g.getFontMetrics.getAscent - anchor_y * b.getHeight).toInt)
  }
}
