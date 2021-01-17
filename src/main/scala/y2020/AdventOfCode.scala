package y2020

import java.io.IOException
import java.nio.file.{Files, Path}
import java.util.Scanner

trait AdventOfCode {
  def applyBoth(): Unit = {
    apply()
    System.out.println(oneLine)
  }

  def apply(): Unit = {
    val sc = new Scanner(Files.newInputStream(file))
    read(sc)
    val s = calculate
    System.out.println(s)
    sc.close()
  }

  def read(sc: Scanner): Unit

  def calculate: String

  def oneLine = ""

  def file: Path
}
