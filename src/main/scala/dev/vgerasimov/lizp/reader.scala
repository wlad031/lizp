package dev.vgerasimov.lizp

import scala.io.Source

import dev.vgerasimov.lizp.syntax.*

def readScript(path: String): Either[LizpError, String] =
  Source.fromFile(path).getLines.mkString("\n").asRight
