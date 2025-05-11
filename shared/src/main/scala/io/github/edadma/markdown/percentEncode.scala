package io.github.edadma.markdown

import java.nio.charset.StandardCharsets

private val unreserved = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-._~:/?=&@+,".toSet

def percentEncode(input: String): String =
  input.flatMap { ch =>
    if (unreserved.contains(ch)) ch.toString
    else {
      // UTF-8 encoding of this character
      val bytes = ch.toString.getBytes(StandardCharsets.UTF_8)
      // for each byte, append “%XX”
      bytes.map { b =>
        "%" + f"${b & 0xff}%02X"
      }.mkString
    }
  }
