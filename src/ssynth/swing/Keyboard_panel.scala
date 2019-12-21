package ssynth.swing

import java.awt.Color

import scala.swing.{Dimension, Graphics2D, Panel}

class Keyboard_panel extends Panel {
  background = Color.WHITE
  preferredSize = new Dimension (Int.MaxValue, 70)

  var notes_playing : Set [Int] = Set ()
  var play_color = Color.YELLOW.brighter ()

  override protected def paintComponent (g : Graphics2D) : Unit = {
    super.paintComponent (g)

    val w = size.width
    val h = size.height
    val key_width = Math.max (20, h / 5)
    def has_black (i : Int) = (i % 7) != 2 && (i % 7) != 6
    val start_x = w / 2 - (10 * 7 + 5) * key_width / 2
    var note = 0
    var i = 0

    while (note <= 129) {
      val x = start_x + i * key_width

      if (notes_playing.contains (note)) {
        g.setColor (play_color)
        g.fillRect (x, 0, key_width, h)
      }

      g.setColor (Color.BLACK)
      g.drawLine (x, 0, x, h)
      note += (if (has_black (i)) 2 else 1)
      i += 1
    }

    note = 0
    i = 0

    while (note < 127) {
      note += 1

      if (has_black (i)) {
        val x = start_x + i * key_width
        g.setColor (if (notes_playing.contains (note)) play_color else Color.BLACK)
        g.fillRect (x + (key_width * 0.7).toInt, 0, (key_width * 0.6).toInt, (h * 0.6).toInt)
        g.setColor (Color.BLACK)
        g.drawRect (x + (key_width * 0.7).toInt, 0, (key_width * 0.6).toInt, (h * 0.6).toInt)
        note += 1
      }

      i += 1
    }
  }
}
