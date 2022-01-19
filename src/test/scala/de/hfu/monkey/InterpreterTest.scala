package de.hfu.monkey

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

class InterpreterTest extends AnyFunSuite {

  private def getListOfFiles(directoryname: String): List[File] = {
    val d = new File(directoryname)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  test("evaluate examples") {
    for (filename <- getListOfFiles("examples").map(file => file.getAbsolutePath)) {
      val actual = Main.execute(filename)
      val lines = scala.io.Source.fromFile(filename).mkString
      val expected = lines.split("//").last.trim
      assert(actual == expected)
    }
  }

}
