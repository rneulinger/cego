package cego

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RBTSpec  extends org.scalatest.flatspec.AnyFlatSpec with Matchers

val PRJ_DIR = java.io.File("").getAbsolutePath
val SRC_DIR = PRJ_DIR + "/src"
val TST_DIR = SRC_DIR + "/test"
val TST_RES = TST_DIR + "/resources"
val TARGET = PRJ_DIR + "/target"


