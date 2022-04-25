package cego

import cego.rbtX.FromRbt

import java.io.File
import cego.scalaX.*


class ImportTest extends RBTSpec {

  it should "BadNodes analyzing" in {
    FromRbt.analyze(s"$TST_RES/BadNodes.rbt")
  }

  it should "dean" in {
    FromRbt.analyze(s"$TST_RES/Canada_University_Gratuation.rbt")
    FromRbt.analyze(s"$TST_RES/Glenford_J_Myers_Example_original.rbt")
    FromRbt.analyze(s"$TST_RES/Glenford_J_Myers_Example_variation_1.rbt")
  }

  it should "small analyzing" in {
    FromRbt.analyze(s"$TST_RES/small.rbt")
  }

  it should "empty rbt file analyzing" in{
    val imp = FromRbt.load(s"$TST_RES/empty.rbt")
  }
  it should "all node types" in{
    println(File( "").getAbsolutePath )
    //Import.load(s"$TST_RES/empty.rbt")
    val imp = FromRbt.load(s"$TST_RES/small.rbt")
    println( imp.meta.mkString("\n") )
    println( imp.title )
    println( imp.Es.mkString("\n") )
    println( imp.Cs.mkString("\n") )
    println( imp.Rs.mkString("\n") )
    println( imp.Ns.mkString("\n") )
    println( imp.Vs.mkString("\n") )
    println( imp.Ms.mkString("\n") )
    val ceg = imp.ceg
    ceg.snap.xy.foreach{ x =>
      println( s"${x._2}   ${x._1}")
    }

  }
  it should "false" in{
    val imp = FromRbt.load(s"$TST_RES/false.rbt")
    val snap = imp.ceg.snap
    println(ToScala(snap).toString)
    snap.xy.foreach{ x =>
      println( s"${x._2}   ${x._1}")
    }
  }

}
