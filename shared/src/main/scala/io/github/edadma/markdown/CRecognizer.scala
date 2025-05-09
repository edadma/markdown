package io.github.edadma.markdown

import io.github.edadma.recognizer.CharRecognizer

case class CRecognizer(cursors: LazyList[C], prev: Option[CRecognizer] = None) extends CharRecognizer[C] {
  def eoi: Boolean = cursors.isEmpty

  def elem: Char = if (eoi) '\u0000' else cursors.head.char

  def wrapped: C = if (eoi) null.asInstanceOf[C] else cursors.head

  def next: CRecognizer = if (eoi) this else CRecognizer(cursors.tail, Some(this))
}
