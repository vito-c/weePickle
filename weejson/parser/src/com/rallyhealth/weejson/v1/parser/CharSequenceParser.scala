package com.rallyhealth.weejson.v1.parser

import com.rallyhealth.weejson.v1.Transformer
import com.rallyhealth.weepickle.v1.core.{Platform, Visitor}
/**
 * Lazy character sequence parsing.
 *
 * This is similar to StringParser, but acts on character sequences.
 */
private[v1] final class CharSequenceParser[J](cs: CharSequence) extends Parser[J] with CharBasedParser[J] {
  var line = 0
  final def column(i: Int) = i
  final def newline(i: Int) = { line += 1 }
  final def dropBufferUntil(i: Int): Unit = ()
  final def char(i: Int): Char = Platform.charAt(cs, i)
  final def sliceString(i: Int, j: Int): CharSequence = cs.subSequence(i, j)
  final def atEof(i: Int) = i == cs.length
  final def close() = ()
}

object CharSequenceParser extends Transformer[CharSequence]{
  def transform[T](j: CharSequence, f: Visitor[_, T]) = new CharSequenceParser(j).parse(f)
}
