package io.github.edadma.markdown

import io.github.edadma.recognizer.Input

case class CInput(cursors: LazyList[C], prev: Option[CInput] = None) extends Input[C, Char] {
  def eoi: Boolean = cursors.isEmpty

  def elem: Char = if (eoi) '\u0000' else cursors.head.char

  def wrapped: C = if (eoi) null.asInstanceOf[C] else cursors.head

  def next: CInput = if (eoi) this else CInput(cursors.tail, Some(this))
}
