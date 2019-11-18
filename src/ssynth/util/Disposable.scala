package ssynth.util

trait Disposable {
  def dispose () : Unit
}

object Disposable {
  val nop: Disposable = () => {}
}
