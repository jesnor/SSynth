package ssynth.swing

import java.awt.event.{InputEvent, MouseEvent, MouseListener, MouseMotionListener}
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

    val insets = 0
    val w = size.width - insets * 2
    val h = size.height - insets * 2
    val cx = w / 2 + insets
    val cy = h / 2 + insets
    val mr = Math.min (w / 2, h / 2)
    val knob_color = background.darker ().darker ().darker ().darker ()
    //    val ring_color = background.darker ()

    def vx (r : Double, v : Double) = (cx + r * Math.cos (to_angle (v))).toInt
    def vy (r : Double, v : Double) = (cy - r * Math.sin (to_angle (v))).toInt
    def draw_marker (r1 : Double, r2 : Double, v : Double) = g.drawLine (vx (r1, v), vy (r1, v), vx (r2, v), vy (r2, v))

    val or = mr * 3 / 4
    val ox = cx - mr
    val oy = cy - mr
    val os = 2 * mr

    //val c = new Color(10, 20, 30)
    //  g setColor background.darker().darker()
    //    g setColor ring_color
    //  g.fillArc (ox, oy, os, os, (end_angle * 180 / Math.PI).toInt, ((start_angle - end_angle) * 180 / Math.PI).toInt)*/
    //  g.setColor (ring_color) //new Color (background.getRed / 2, background.getGreen / 2, 255))
    // g.fillArc (ox, oy, os, os, (to_angle (value) * 180 / Math.PI).toInt, ((start_angle - to_angle (value)) * 180 / Math.PI).toInt)
    //    g.fillArc (ox, oy, os, os, (to_angle (value) * 180 / Math.PI).toInt, ((start_angle - to_angle (value)) * 180 / Math.PI).toInt)

    /*    g setColor foreground.darker().darker()
        g.drawOval (ox, oy, os, os)
        draw_marker (1, min)
        draw_marker (1, max)
    */
    g setColor foreground
    draw_marker (mr, 0, min)
    draw_marker (mr, 0, max)

    g setColor knob_color
    g.fillOval (cx - or, cy - or, or * 2, or * 2)
    g setStroke new BasicStroke (3, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER)
    g setColor Color.WHITE
    draw_marker (or, or * 0.3, value)
  }

  def set_value (v : Double) = {
    value = v
    action (v)
    repaint ()
  }

  private var start : Option [(Point, Double, Int)] = None

  override def mouseEntered (e : MouseEvent) = {}
  override def mouseExited (e : MouseEvent) = {}
  override def mouseMoved (e : MouseEvent) = {}

  override def mouseClicked (e : MouseEvent) =
    if (enabled && e.getButton == MouseEvent.BUTTON1 && e.getClickCount == 2) set_value (default_value)

  override def mousePressed (e : MouseEvent) = {
    if (enabled)
      e.getButton match {
        case MouseEvent.BUTTON1 | MouseEvent.BUTTON3 =>
          start = Some (e.getPoint, value, e.getButton)

        // Abort and reset on right click
        /*        case MouseEvent.BUTTON3 if enabled =>
                  for ((_, v) <- start) {
                    set_value (v)
                    start = None
                  }
        */
        case _ =>
      }
  }

  override def mouseReleased (e : MouseEvent) =
    if (e.getButton == MouseEvent.BUTTON1 || e.getButton == MouseEvent.BUTTON3) start = None

  override def mouseDragged (e : MouseEvent) =
    for ((p, v, b) <- start) {
      if (enabled) {
        val delta = e.getPoint.x - p.x + p.y - e.getPoint.y
        val div = if ((e.getModifiers & InputEvent.SHIFT_MASK) != 0 || b == MouseEvent.BUTTON3) 800.0 else 200.0
        set_value (to_value (to_gui (v) + delta / div))
        start = Some ((e.getPoint, value, b))
        action (value)
        repaint ()
      }
    }
}
