package ssynth.swing

import java.awt.event.{MouseEvent, MouseListener, MouseMotionListener}
import java.awt.{BasicStroke, Color, Point, RenderingHints}

import scala.swing.{Dimension, Graphics2D, Panel}

class Knob (var min : Double,
            var max : Double,
            var value : Double,
            val action : Double => Unit,
            var exp : Double = 1,
            var start_angle : Double = Math.PI * 5 / 4,
            var end_angle : Double = -Math.PI / 4
           ) extends Panel with MouseListener with MouseMotionListener {
  def to_gui (v : Double) = Math.pow ((v - min) / (max - min), 1.0 / exp)
  def to_value (v : Double) = (Math.pow (v.max (0).min (1), exp) * (max - min) + min).min (max).max (min)
  def to_angle (v : Double) = to_gui (v) * (end_angle - start_angle) + start_angle

  var default_value = value

  preferredSize = new Dimension (50, 50)
  peer addMouseListener this
  peer addMouseMotionListener this

  override def paintComponent (g : Graphics2D) : Unit = {
    super.paintComponent (g)
    g.setRenderingHint (RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    val insets = 4
    val w = size.width - insets * 2
    val h = size.height - insets * 2
    val cx = w / 2 + insets
    val cy = h / 2 + insets
    val mr = Math.min (w / 2, h / 2)

    def vx (r : Double, v : Double) = (cx + r * mr * Math.cos (to_angle (v))).toInt
    def vy (r : Double, v : Double) = (cy - r * mr * Math.sin (to_angle (v))).toInt
    def draw_marker (r : Double, v : Double) = g.drawLine (cx, cy, vx (r, v), vy (r, v))

    val or = mr * 7 / 8
    val ox = cx - or
    val oy = cy - or
    val os = 2 * or

    //val c = new Color(10, 20, 30)
    //  g setColor background.darker().darker()
    val c = background.darker ()
    g setColor c
    g.fillArc (ox, oy, os, os, (end_angle * 180 / Math.PI).toInt, ((start_angle - end_angle) * 180 / Math.PI).toInt)
    g.setColor (new Color (c.getRed, c.getGreen, Math.min (255, c.getBlue * 2)))
    g.fillArc (ox, oy, os, os, (to_angle (value) * 180 / Math.PI).toInt, ((start_angle - to_angle (value)) * 180 / Math.PI).toInt)
    //    g.fillArc (ox, oy, os, os, (to_angle (value) * 180 / Math.PI).toInt, ((start_angle - to_angle (value)) * 180 / Math.PI).toInt)

    /*    g setColor foreground.darker().darker()
        g.drawOval (ox, oy, os, os)
        draw_marker (1, min)
        draw_marker (1, max)
    */
    g setStroke new BasicStroke (4, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER)
    g setColor foreground
    g.fillOval (cx - mr / 4, cy - mr / 4, mr / 2, mr / 2)
    draw_marker (1, value)
  }

  def set_value (v : Double) = {
    value = v
    action (v)
    repaint ()
  }

  private var start : Option [(Point, Double)] = None

  override def mouseEntered (e : MouseEvent) = {}
  override def mouseExited (e : MouseEvent) = {}
  override def mouseMoved (e : MouseEvent) = {}

  override def mouseClicked (e : MouseEvent) =
    if (enabled && e.getButton == MouseEvent.BUTTON1 && e.getClickCount == 2) set_value (default_value)

  override def mousePressed (e : MouseEvent) = {
    if (enabled)
      e.getButton match {
        case MouseEvent.BUTTON1 =>
          start = Some (e.getPoint, value)

        // Abort and reset on right click
        case MouseEvent.BUTTON3 if enabled =>
          for ((_, v) <- start) {
            set_value (v)
            start = None
          }

        case _ =>
      }
  }

  override def mouseReleased (e : MouseEvent) =
    if (e.getButton == MouseEvent.BUTTON1) start = None

  override def mouseDragged (e : MouseEvent) =
    for ((p, v) <- start) {
      if (enabled) {
        val delta = e.getPoint.x - p.x + p.y - e.getPoint.y
        set_value (to_value (to_gui (v) + delta / 200.0))
        action (value)
        repaint ()
      }
    }
}
