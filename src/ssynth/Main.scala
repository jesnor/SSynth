package ssynth

import java.awt.event.KeyEvent
import java.awt.{BasicStroke, Color, KeyboardFocusManager}

import javax.sound.midi.{MidiMessage, Receiver}
import javax.swing.filechooser.FileNameExtensionFilter
import javax.swing.{SwingUtilities, Timer}
import scala_utils.audio.utils
import scala_utils.swing.utils._
import scala_utils.utils.Array_float
import ssynth.swing.{Graph_panel, Keyboard_panel}

import scala.collection.mutable
import scala.swing._

object Main extends SimpleSwingApplication {
  set_nimbus_look_and_feel ()

  val sample_rate = 44100
  val nyquist_freq = sample_rate / 2.0

  val line = utils.open_line (sample_rate)

  val buffer = new Array [Byte](line.getBufferSize)
  val sample_buffer = new Array [Float](line.getBufferSize)

  var display_tone = 55.0

  val synth_module = new Synth_params (nyquist_freq)
  val synth = new Synth (synth_module, nyquist_freq)

  var clip_amount = 0

  def fill_sound_buffer () {
    val count = line.available () / 2

    if (count > 0) {
      synth.mix_voices (sample_rate, sample_buffer, 0, count)

      for (i <- 0 until count) {
        var s = (sample_buffer (i) * 16000).toInt

        if (s > 32767) {
          clip_amount = clip_amount max (s - 32767)
          s = 32767
        }
        else if (s < -32768) {
          clip_amount = clip_amount max (-32768 - s)
          s = -32768
        }

        buffer (i * 2) = s.toByte
        buffer (i * 2 + 1) = (s >> 8).toByte
      }

      line.write (buffer, 0, count * 2)
    }
  }

  val info_label = new Label
  val synth_panel = swing.parameter_group_panel (synth_module)
  val keyboard_panel = new Keyboard_panel((note, playing) => if (playing) play_note(note) else stop_playing_note(note))
  var spectrum : Array_float = _

  val spectrum_panel = new Panel {
    val graph_inset = 10
    val graph_bg = new Color (10, 20, 40)
    val graph_grid_color = graph_bg.brighter ().brighter ()
    val graph_bright_grid_color = graph_grid_color.brighter ().brighter ()
    val graph_line_color = Color.GREEN
    val graph_fill_color = Color.GREEN.darker ().darker ().darker ().darker ()
    val graph_filter_color = Color.CYAN
    val graph_thick_stroke = new BasicStroke (2)

    val db_range = 100
    val bar_stroke = new BasicStroke (3, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER)
    background = graph_bg

    override protected def paintComponent (g : Graphics2D) : Unit = {
      super.paintComponent (g)

      val stroke = g.getStroke
      val w = size.width - graph_inset * 2
      val h = size.height - graph_inset * 2

      g.setClip (graph_inset - 1, graph_inset - 1, w + 2, h + 2)

      val freq_count = synth.max_harmonics_count

      def amp (k : Int) = {
        val a = spectrum (k * 2)
        val b = spectrum (k * 2 + 1)
        Math.sqrt (a * a + b * b)
      }

      def graph_to_harmonic_freq (x : Double) = synth.to_harmonic_freq (graph_to_normalized_freq (x))
      def graph_to_normalized_freq (x : Double) = x / w
      def amp_to_graph (a : Double) = if (a == 0) 1000.0 else 0.2 - 10 * Math.log10 (a * a) / db_range

      def harmonic_to_x (k : Double) = normalized_freq_to_x (synth.to_normalized_freq (k))
      def normalized_freq_to_x (f : Double) = (f * w).toInt + graph_inset
      def amp_to_y (a : Double) = (amp_to_graph (a) * h).toInt + graph_inset

      val bottom_y = h + graph_inset

      g setColor graph_grid_color
      g drawRect (graph_inset, graph_inset, w, h)
      var k = 1

      // X-axis
      while (k < freq_count) {
        val x = harmonic_to_x (k)
        g.setColor (graph_grid_color)
        g drawLine (x, graph_inset, x, bottom_y)
        g.setColor (graph_bright_grid_color)
        draw_string (g, k.toString, x + 4, graph_inset + 4)
        k *= 2
      }

      // Y-axis
      for (a <- -1 until db_range / 10) {
        val y = amp_to_y (Math.pow (10, -a.toDouble / 2))

        if (a == 0) {
          g setColor graph_bright_grid_color
          g setStroke graph_thick_stroke
        }
        else {
          g.setColor (graph_grid_color)
          g setStroke stroke
        }

        g drawLine (graph_inset, y, size.width - graph_inset, y)
        g.setColor (graph_bright_grid_color)
        g setStroke stroke
        draw_string (g, (-a * 10) + " dB", graph_inset + w - 4, y + 4, 1)
      }

      // Filter response
      val fx = normalized_freq_to_x (synth_module.filter_control.freq ())
      g setStroke graph_thick_stroke
      g.setColor (graph_filter_color)
      g drawLine (fx, amp_to_y (1), fx, graph_inset + h)

      for (x1 <- 0 until w by 4) {
        val x2 = x1 + 4
        val f1 = graph_to_harmonic_freq (x1)
        val f2 = graph_to_harmonic_freq (x2)
        val y1 = amp_to_y (synth.filter_gain (f1))
        val y2 = amp_to_y (synth.filter_gain (f2))

        if ((y1 >= graph_inset && y1 < graph_inset + h) || (y2 >= graph_inset && y2 < graph_inset + h))
          g drawLine (graph_inset + x1, y1, graph_inset + x2, y2)
      }

      // Harmonics
      g setColor graph_line_color
      g setStroke bar_stroke

      for (k <- 1 to synth_module.harmonics.count ()) {
        val x = harmonic_to_x (k)
        val y = amp_to_y (amp (k))

        if (y < graph_inset + h)
          g drawLine (x, y, x, bottom_y)
      }
    }
  }

  val wave_panel = new Graph_panel

  val frame = main_frame ("FFT Synth v0.1", new Dimension (1024, 768), top_center_panel (
    hbox (
      /*toggle_button ("Play", false, b =>
        if (b) {
          //          synth.play_tone (tone)
          fill_sound_buffer ()
          timer.start ()
        } else
          timer.stop ()
      ),*/

      //button ("Save sample", () => save_sample_buffer ("sample")),

      info_label
    ),

    top_center_panel (
      synth_panel,

      new BorderPanel {
        add (hbox (
          titled_panel ("Power Spectrum", spectrum_panel),
          titled_panel ("Output Sample", wave_panel)
        ), BorderPanel.Position.Center)

        add (titled_panel ("Keyboard", keyboard_panel), BorderPanel.Position.South)
      }
    )))
  def top = frame

  def update_info_label () = {
    info_label.text =
        "Voices: " + synth.voices.size +
    synth.voices.headOption.map(", freq: " + _.freq.toInt.toString + " Hz").getOrElse("") +
            ", spectrum time: " + synth.spectrum_time / 1000 + " us, FFT time: " + synth.fft_time / 1000 + " us" +
            (if (clip_amount > 0) ", clipped: " + clip_amount else "")
  }

  val timer = new Timer (1, _ => {
    clip_amount = 0
    fill_sound_buffer ()
    update_info_label ()
  })

  val synth_observer = synth.now_and_on_change {
    spectrum = synth.get_samples (display_tone)._1
    wave_panel.set_data (synth.get_samples (display_tone)._2)
    wave_panel.repaint ()
    spectrum_panel.repaint ()
  }

  val chooser = new FileChooser
  chooser.fileFilter = new FileNameExtensionFilter ("WAV files", "wav")

  def save_sample_buffer (filename : String) : Unit = {
    val returnVal = chooser.showSaveDialog (frame)

    if (returnVal == FileChooser.Result.Approve) {
      /*      val s1 = new ByteArrayInputStream (synth.sampleBuffer, 0, synth.sampleBufferPos)

            val s =
              if (synth.sampleBufferPos - synth.sampleSize < 0)
                new SequenceInputStream (new ByteArrayInputStream (synth.sampleBuffer, synth.sampleBufferPos, synth.sampleBuffer.length - synth.sampleBufferPos), s1)
              else
                s1

            AudioSystem.write (new AudioInputStream (s, line.getFormat, synth.sampleSize), AudioFileFormat.Type.WAVE, chooser.selectedFile)*/
    }
  }

  var keyboard_start = 48
  val note_keys = "zsxdcvgbhnjmq2w3er5t6y7ui9o0p"
  var keys_playing = new mutable.HashMap [Char, Int]

  def key_to_note (c : Char) = {
    val i = note_keys.indexOf (c)
    if (i == -1) None else Some (keyboard_start + i)
  }

  def play_note (n : Int): Unit = {
    synth.play_note (n)
    keyboard_panel.notes_playing = synth.notes_playing.keys.toSet
    keyboard_panel.repaint ()
  }

  def stop_playing_note (n : Int):Unit = {
    synth.stop_playing_note (n)
    keyboard_panel.notes_playing = synth.notes_playing.keys.toSet
    keyboard_panel.repaint ()
  }

  KeyboardFocusManager.getCurrentKeyboardFocusManager.addKeyEventDispatcher ((e : KeyEvent) => {
    e.getID match {
      case KeyEvent.KEY_PRESSED =>
        if (!keys_playing.contains (e.getKeyChar)) {
          for {
            n <- key_to_note (e.getKeyChar)
          } {
            play_note (n)
            keys_playing (e.getKeyChar) = n
          }
        }

      case KeyEvent.KEY_RELEASED =>
        for {
          n <- keys_playing.get (e.getKeyChar)
        } {
          stop_playing_note (n)
          keys_playing.remove (e.getKeyChar)
        }

      case _ =>
    }

    false
  })

  def register_midi_devices () : Unit = {
    import javax.sound.midi.{MidiSystem, MidiUnavailableException}

    for (info <- MidiSystem.getMidiDeviceInfo) {
      try {
        val device = MidiSystem.getMidiDevice (info)
        val transmitters = device.getTransmitters

        val r = new Receiver {
          override def send (m : MidiMessage, timeStamp : Long) = m.getStatus >> 4 match {
            case 9 =>
              val note = m.getMessage ()(1)
              SwingUtilities.invokeLater (() => play_note (note))

            case 8 =>
              val note = m.getMessage ()(1)
              SwingUtilities.invokeLater (() => stop_playing_note (note))

            case 11 =>
              val control = m.getMessage ()(1)
              val value = m.getMessage ()(2)

              SwingUtilities.invokeLater (() => control match {
                case 21 => synth_module.filter_control.freq () = value / 127.0
              })

            case _ =>
          }

          override def close () = {}
        }

        for (i <- 0 until transmitters.size ())
          transmitters.get (i).setReceiver (r)

        device.getTransmitter.setReceiver (r)
        device.open ()
      }
      catch {
        case _ : MidiUnavailableException =>
      }
    }
  }

  register_midi_devices ()
  line.start ()
  fill_sound_buffer ()
  timer.start ()
}
