package ssynth

import java.awt.{BasicStroke, Color}

import javax.swing.Timer
import javax.swing.filechooser.FileNameExtensionFilter
import org.jtransforms.fft.FloatFFT_1D
import ssynth.swing.utils.{knob_with_labels, _}
import ssynth.util.utils.Array_float

import scala.swing._

object Main extends SimpleSwingApplication {
  set_nimbus_look_and_feel ()

  val frame_size = 1 << 11
  val sample_rate = 44100
  val nyquist_freq = sample_rate / 2.0
  val fft = new FloatFFT_1D (frame_size)
  var tone = 55.0f
  var tone_harmonic_count = (nyquist_freq / tone).toInt

  var saw_square = 0.0
  var pulse_width = 0.5
  var max_harmonic_count = tone_harmonic_count
  var osc_sinc_exp = 1.5
  var osc_slope = 1.0

  var unison_count = 1
  var unison_spread = 1.0

  var filter_enabled = false
  var filter_freq = harmonic_count (tone).toDouble
  var filter_slope = 12.0

  var sinc_filter_enabled = true
  var sinc_filter_freq_muliplier = 1.0
  var sinc_filter_exp = 100.0
  var sinc_filter_cutoff = calc_sinc_filter_cutoff

  val synth = new Synth (generate_samples)
  val spectrum = new Array_float (frame_size)
  val samples = new Array_float (frame_size)
  var amp_max = 1.0

  var spectrum_time = 0L
  var fft_time = 0L

  def harmonic_count (freq : Float) = (nyquist_freq / freq).toInt

  def linear_filter_gain (f : Double) = Math.min (1.0, Math.pow (filter_freq / f, filter_slope / 6))

  def calc_sinc_filter_cutoff = math.sinc_normalized_inverse (Math.pow (1.0 / Math.sqrt (2), 1 / sinc_filter_exp))

  def sinc_filter_gain (f : Double) = Math.pow (
    math.sinc_normalized (Math.min (1, (f - 1) / (sinc_filter_freq_muliplier * filter_freq / sinc_filter_cutoff))),
    sinc_filter_exp)

  def filter_gain (f : Double) = {
    val lf = if (filter_enabled) linear_filter_gain (f) else 1.0
    val sf = if (sinc_filter_enabled) sinc_filter_gain (f) else 1.0
    lf * sf
  }

  def time (f : => Unit) = {
    val t = System.nanoTime ()
    f
    System.nanoTime () - t
  }

  def generate_samples (f : Float) : Array_float = {
    val s = new Array_float (frame_size)
    val hc = harmonic_count (f).min (max_harmonic_count)
    var amp = 0.0
    var amp1 = 0.0

    spectrum_time = time {
      for (i <- 1 to max_harmonic_count) {
        amp += Math.pow (i, -osc_slope)
        amp1 += 1.0 / i
      }

      for (i <- 1 to hc) {
        val saw_a = 0
        val saw_p = 2 / Math.PI * Math.sin (Math.PI * (i * (0.5 - pulse_width) - 0.5))
        val square_a = 4 / Math.PI * Math.sin (Math.PI * i * pulse_width)
        val square_p = 0.0
        val a1 = math.interpolate (saw_square, saw_a, square_a) * Math.pow (i, -osc_slope)
        val a2 = math.interpolate (saw_square, saw_p, square_p) * Math.pow (i, -osc_slope)
        val gc = Math.pow (math.sinc_normalized (i.toDouble / (hc + 1)), osc_sinc_exp)
        val gain = gc * filter_gain (i)
        s (i * 2) = (gain * a1).toFloat
        s (i * 2 + 1) = (gain * a2).toFloat
      }
    }

    s.copyToArray (spectrum)
    fft_time = time (fft.realInverse (s, false))
    val scale = (amp1 / amp).toFloat

    for (i <- samples.indices)
      s (i) *= scale

    s.copyToArray (samples)
    update_info_label ()
    s
  }

  def update_synth () : Unit = {
    generate_samples (tone)
    synth.update_voices ()
    wave_panel.repaint ()
    spectrum_panel.repaint ()
  }

  var clip_amount = 0
  val line = audio.utils.open_line (sample_rate)
  val graph_inset = 10
  val graph_bg = new Color (10, 20, 40)
  val graph_grid_color = graph_bg.brighter ().brighter ()
  val graph_line_color = Color.GREEN
  val graph_fill_color = Color.GREEN.darker ().darker ().darker ().darker ()

  val spectrum_panel = new Panel {
    val db_range = 100
    val bar_stroke = new BasicStroke (3, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER)
    background = graph_bg

    override protected def paintComponent (g : Graphics2D) : Unit = {
      super.paintComponent (g)

      val w = size.width - graph_inset * 2
      val h = size.height - graph_inset * 2

      g.setClip (graph_inset - 1, graph_inset - 1, w + 2, h + 2)

      val freq_count = spectrum.length / 2 - 1

      def amp (k : Int) = {
        val a = spectrum (k * 2)
        val b = spectrum (k * 2 + 1)
        Math.sqrt (a * a + b * b)
      }

      def graph_to_freq (x : Double) = Math.pow (freq_count, x / w)
      def freq_to_graph (k : Double) = Math.log (k) / Math.log (freq_count)
      def amp_to_graph (a : Double) = if (a == 0) 1000.0 else 0.1 - 10 * Math.log10 (a * a) / db_range

      def freq_to_x (k : Double) = (freq_to_graph (k) * w).toInt + graph_inset
      def amp_to_y (a : Double) = (amp_to_graph (a) * h).toInt + graph_inset

      val bottom_y = h + graph_inset

      g setColor graph_grid_color
      g drawRect (graph_inset, graph_inset, w, h)
      var last_x = -100
      var step = 1
      var k = 0

      // X-axis
      while (k < freq_count) {
        val x = freq_to_x (k)

        if (last_x >= 0 && x < last_x + 4)
          step *= 10

        g drawLine (x, graph_inset, x, bottom_y)
        last_x = x
        k += step
      }

      // Y-axis
      for (a <- 0 until 100) {
        val y = amp_to_y (Math.pow (0.1, a.toDouble / 2))
        g drawLine (graph_inset, y, size.width - graph_inset, y)
      }

      // Filter response
      g setColor graph_grid_color.brighter ().brighter ()
      val fx = freq_to_x (filter_freq)
      g drawLine (fx, amp_to_y (1), fx, graph_inset + h)

      for (x1 <- 0 until w by 4) {
        val x2 = x1 + 4
        val f1 = graph_to_freq (x1)
        val f2 = graph_to_freq (x2)
        val y1 = amp_to_y (filter_gain (f1))
        val y2 = amp_to_y (filter_gain (f2))

        if ((y1 >= graph_inset && y1 < graph_inset + h) || (y2 >= graph_inset && y2 < graph_inset + h))
          g drawLine (graph_inset + x1, y1, graph_inset + x2, y2)
      }

      // Harmonics
      g setColor graph_line_color
      g setStroke bar_stroke

      for (k <- 1 to max_harmonic_count) {
        val x = freq_to_x (k)
        val y = amp_to_y (amp (k))

        if (y < graph_inset + h)
          g drawLine (x, y, x, bottom_y)
      }
    }
  }

  val wave_panel = new Panel {
    background = graph_bg

    override protected def paintComponent (g : Graphics2D) : Unit = {
      super.paintComponent (g)

      val w = size.width - graph_inset * 2
      val h = size.height - graph_inset * 2

      def i_to_x (i : Int) = graph_inset + i * w / samples.length
      def amp_to_y (a : Double) = h / 2 + (-a * h / 4).toInt + graph_inset

      g setColor graph_grid_color
      g drawRect (graph_inset, graph_inset, w, h)
      val mid_x = graph_inset + w / 2
      g drawLine (mid_x, graph_inset, mid_x, graph_inset + h)

      for (i <- -1 to 1) {
        val y = amp_to_y (i)
        g drawLine (graph_inset, y, graph_inset + w, y)
      }

      g setColor graph_line_color
      g setStroke new BasicStroke (2)

      for (i <- samples.indices.tail) {
        val x1 = i_to_x (i - 1)
        val y1 = amp_to_y (samples (i - 1))
        val x2 = i_to_x (i)
        val y2 = amp_to_y (samples (i))
        g drawLine (x1, y1, x2, y2)
      }
    }
  }

  //val rt = new Timer (250, _ => wavePanel.repaint ())
  //rt.start ()

  val buffer = new Array [Byte](line.getBufferSize)
  val sample_buffer = new Array [Float](line.getBufferSize)
  val info_label = new Label

  def fill_sound_buffer () {
    val count = line.available () / 2

    if (count > 0) {
      synth.generate_samples (sample_rate, sample_buffer, 0, count)

      for (i <- 0 until count) {
        var s = (sample_buffer (i) * 10000).toInt

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

  val timer = new Timer (1, _ => {
    clip_amount = 0
    fill_sound_buffer ()
    update_info_label ()
  })

  def update_info_label () = {
    info_label.text =
        "Voices: " + synth.voices.size +
            ", spectrum time: " + spectrum_time / 1000 + " us, FFT time: " + fft_time / 1000 + " us"
            (if (clip_amount > 0) ", clipped: " + clip_amount else "")
  }

  val frame = main_frame ("FFT Synth v0.1", new Dimension (800, 600), top_center_panel (
    hbox (
      toggle_button ("Play", false, b =>
        if (b) {
          synth.play_tone (tone)
          fill_sound_buffer ()
          timer.start ()
        } else
          timer.stop ()),

      //button ("Save sample", () => save_sample_buffer ("sample")),

      info_label
    ),

    top_center_panel (
      hbox (flow_panel (
        titled_panel ("Osc", flow_panel (
          knob_with_labels ("Saw/square", 0, 100, saw_square * 100, v => {
            saw_square = v / 100.0
            update_synth ()
          }),

          knob_with_labels ("PW", 0, 100, pulse_width * 100, v => {
            pulse_width = v / 100.0f
            update_synth ()
          }),

          /*          knob_with_labels ("Freq", 20, 8000, tone, v => {
                      //            harmonic_count = v
                      //          update_synth ()
                    }),
          */
          knob_with_labels ("Harmonics", 1, max_harmonic_count, max_harmonic_count, v => {
            max_harmonic_count = v.round.toInt
            update_synth ()
          }, 6),

          knob_with_labels ("Slope", 0, 100, 100 - osc_slope * 100, v => {
            osc_slope = 1 - v / 100
            update_synth ()
          }),

          knob_with_labels ("Gibbs", 0, 300, osc_sinc_exp * 100, v => {
            osc_sinc_exp = v / 100
            update_synth ()
          })
        )
        ),

        /*titled_panel ("Unison", flow_panel (
          knob_with_labels ("Count", 1, 10, unison_count, v => {
            unison_count = v.toInt
            update_synth ()
          }),

          knob_with_labels ("Spread", 0, 100, unison_spread, v => {
            unison_spread = v
            update_synth ()
          })
        )),*/

        titled_panel ("LP Filter", flow_panel (
          knob_with_labels ("Freq", 1, filter_freq, filter_freq, v => {
            filter_freq = v
            update_synth ()
          }, 7),
        )),

        {
          val filter_panel = flow_panel (
            knob_with_labels ("Slope", 0, 100, filter_slope, v => {
              filter_slope = v
              sinc_filter_cutoff = calc_sinc_filter_cutoff
              update_synth ()
            })
          )

          titled_panel (
            check_box ("Linear Filter", filter_enabled, s => {
              filter_panel.enabled = s
              filter_enabled = s
              update_synth ()
            }),

            filter_panel
          )
        },

        {
          val sinc_filter_panel = flow_panel (
            knob_with_labels ("Cutoff mult", 1, 20, sinc_filter_freq_muliplier, v => {
              sinc_filter_freq_muliplier = v
              update_synth ()
            }, 2),

            knob_with_labels ("Slope", 30, 10000, sinc_filter_exp * 100, v => {
              sinc_filter_exp = v / 100
              sinc_filter_cutoff = calc_sinc_filter_cutoff
              update_synth ()
            }, 3)
          )

          titled_panel (
            check_box ("Sinc Filter", sinc_filter_enabled, s => {
              sinc_filter_panel.enabled = s
              sinc_filter_enabled = s
              update_synth ()
            }),

            sinc_filter_panel
          )
        }
      )),

      hbox (
        titled_panel ("Spectrum", spectrum_panel),
        titled_panel ("Wave", wave_panel)
        //    hboxs (synth.chains.map (c =>
        //    vboxs (c.modules.map (create_panel))) :+ create_panel (synth.mixer))
      ))))

  def top = frame

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

  generate_samples (tone)
  line.start ()
}
