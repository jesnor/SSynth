package ssynth.swing

import java.awt.{BasicStroke, Color}

import scala_utils.math.{Range, Range_float}

import scala.swing.{Graphics2D, Panel}

class Graph_panel extends Panel {
  val graph_inset = 10
  val graph_line_color = Color.GREEN
  val graph_thick_stroke = new BasicStroke (2)
  val graph_bg = new Color (10, 20, 40)
  val graph_grid_color = graph_bg.brighter ().brighter ()

  private var data : Seq [Float] = Seq ()
  private var range : Range_float = Range_float.zero

  def set_data (d : Seq [Float]) = {
    range = Range (d.min, d.max)
    data = d
    repaint ()
  }

  background = graph_bg

  override protected def paintComponent (g : Graphics2D) : Unit = {
    super.paintComponent (g)

    val w = size.width - graph_inset * 2
    val h = size.height - graph_inset * 2
    val s = range.max.max (-range.min)

    def i_to_x (i : Int) = graph_inset + i * w / data.size
    def amp_to_y (a : Double) = (0.5 * h * (1 + -a / s)).toInt + graph_inset

    g setColor graph_grid_color
    g drawRect (graph_inset, graph_inset, w, h)
    val mid_x = graph_inset + w / 2
    g drawLine (mid_x, graph_inset, mid_x, graph_inset + h)

    for (i <- -1 to 1) {
      val y = amp_to_y (i)
      g drawLine (graph_inset, y, graph_inset + w, y)
    }

    g setColor graph_line_color
    g setStroke graph_thick_stroke

    for (i <- data.indices.tail) {
      val x1 = i_to_x (i - 1)
      val y1 = amp_to_y (data (i - 1))
      val x2 = i_to_x (i)
      val y2 = amp_to_y (data (i))
      g drawLine (x1, y1, x2, y2)
    }
  }
}
