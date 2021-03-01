package ssynth.swing

import java.awt.Color
import java.awt.event.{InputEvent, MouseEvent, MouseListener}

import scala.swing.{Dimension, Graphics2D, Panel}

class Keyboard_panel(note_fn: (Int, Boolean) => Unit) extends Panel with MouseListener {
  background = Color.WHITE
  preferredSize = new Dimension (Int.MaxValue, 70)
  var mouse_note: Option[Int] = None
  var mouse_hold_notes: Set[Int] = Set()

  var notes_playing : Set [Int] = Set ()
  var play_color = Color.YELLOW.brighter ()

  def has_black (i : Int) = (i % 7) != 2 && (i % 7) != 6
  def key_width = Math.max (20, size.height / 5)
  def start_x = size.width / 2 - (10 * 7 + 5) * key_width / 2

  def white_key_to_note (i: Int) = {
    val oct_i = i % 7
    i / 7 * 12 + oct_i * 2 - (if (oct_i > 2) 1 else 0)
  }

  def x_to_note(x: Int) = white_key_to_note((x - start_x) / key_width)
  def x_to_white_note(x: Int) = (x - start_x) / key_width

  override protected def paintComponent (g : Graphics2D) : Unit = {
    super.paintComponent (g)

    val h = size.height
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

  override def mousePressed(e: MouseEvent): Unit = {
    if (e.getButton == MouseEvent.BUTTON1) {
      val note = x_to_note(e.getX)

      if (note >= 0 && note < 128) {
        if ((e.getModifiers & InputEvent.SHIFT_MASK) != 0)
          note_fn(note, !notes_playing(note))
        else {
          mouse_note = Some(note)
          note_fn(note, true)
        }
      }
    }
  }

  override def mouseReleased(e: MouseEvent): Unit = {
    if (e.getButton == MouseEvent.BUTTON1) {
      for (note <- mouse_note)
        note_fn(note, false)
    }
  }

  override def mouseEntered(e: MouseEvent): Unit = {}
  override def mouseExited(e: MouseEvent): Unit = {}
  peer addMouseListener this

  override def mouseClicked(e: MouseEvent): Unit = {}
}
