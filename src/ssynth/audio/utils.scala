package ssynth.audio

import javax.sound.sampled.{AudioFormat, AudioSystem, DataLine, SourceDataLine}

object utils {
  def open_line (sample_rate : Float = 44100, bits : Int = 16, mono : Boolean = true) : SourceDataLine = {
    val format = new AudioFormat (sample_rate, bits, if (mono) 1 else 2, true, false)
    val info = new DataLine.Info (classOf [SourceDataLine], format)

    if (!AudioSystem.isLineSupported (info))
      throw new Exception ("Format not supported!")

    // Obtain and open the line.
    val line = AudioSystem.getLine (info).asInstanceOf [SourceDataLine]
    line open (format, 30000)
    line
  }
}
