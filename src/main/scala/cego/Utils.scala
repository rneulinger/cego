package cego

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

def mkUUID:String = java.util.UUID.randomUUID.toString.filterNot(_ == '-')

case class Pos(x: Int, y: Int):
  def this(x: String, y: String) = this(x.toInt, y.toInt)

class Register:
  private var store: Map[String, Int] = Map()

  def add(x: String): Int =
    if store.contains(x) then
      val res = store(x) + 1
      store = store + (x -> res)
      res
    else
      store = store + (x -> 1)
      1

/**
 *
 * @param n  how many elements are required.
 *           if n > 0 then n else 0
 * @param ts element
 * @tparam T type of required elements
 * @return Set with size >= n
 * @throws IllegalArgumentException in less than n element
 */
def atLeast[T](n: Int, ts: Seq[T]): Set[T] = atLeast(n, ts.toSet)
def atLeast[T](n: Int, ts: Set[T]) =
  val req = if n < 0 then 0 else n
  require(ts.size >= req, s"At least $req element(s) required ${ts.size} >= $req $ts")
  ts

/**
 * one or more elements of a given type
 *
 * @param t    required element
 * @param more if there are more than 1 element
 * @tparam T type of element
 * @return Set with size >= 1
 * @throws IllegalArgumentException in less than 1 element
 */
inline def oneOrMore[T](t: T, more: T*): Set[T] = atLeast(1, Seq(t) ++ more)
inline def oneOrMore[T](es: Set[T]): Set[T] = atLeast(1, es)


/**
 * two or more elements of a given type
 *
 * @param e1   first element
 * @param e2   second element
 * @param more if there are more than 2 elements
 * @tparam T type of element
 * @return Set with at least two elements
 * @throws IllegalArgumentException if less than 2 elements
 */
inline def twoOrMore[T](e1: T, e2: T, more: T*): Set[T] = atLeast(2, Seq(e1, e2) ++ more)
inline def twoOrMore[T](es: Set[T]): Set[T] = atLeast(2, es)

def ensureNoDuplicates[T](elements: Seq[T]): Set[T] =
  val set = Set(elements *)
  if set.size != elements.size then throw IllegalArgumentException("OR contains duplicates")
  set


var LOGLEVEL = 1 //


/**
 * a few utilities for class string
 */

extension (str: String)

/**
 * return the next unique string py appending "_$n".
 */
  def mkUnique(existing: Set[String], proposal: String): String =
    inline def isUnique(s: String) = !existing.contains(s)

    @tailrec
    def recurse(n: Int): String =
      val attempt = proposal + s"_$n"
      if isUnique(attempt) then attempt
      else recurse(n + 1)

    if isUnique(proposal) then proposal
    else recurse(1)
  /**
   * replace multiple whitespace with one blank
   */
  inline def singleWhitespace: String = str.replaceAll("\\s+", " ")

  /**
   * replace multiple whitespace from string (can be bultiline).
   * if preserveLeading is set to true leading whitespace of each line is preserved.
   */
  def normalize(preserveIndent: Boolean = false): String =
    str.linesIterator.map { line =>
      if (preserveIndent) {
        val indent = line.takeWhile(_.isWhitespace)
        indent + line.drop(indent.length).singleWhitespace
      } else line.singleWhitespace
    }.mkString("\n")

  def ReplacementForEmptyNames = "bac7b380e1bf4e5ea6ce292246019976"

  /**
   * very simple function to create a valid scala name.
   * TODO wrap _
   *
   * @return
   *
   */
  def toScalaName: String =
    val nrm = str.trim.replaceAll("\\s+", "_")
    val regEx = "([a-zA-Z_][a-zA-Z0-9_]*)".r
    nrm match{
      case "" => ReplacementForEmptyNames
      case "_" => "`_`"
      case regEx(x) => x
      case _ => s"`$nrm`"
    }

  def toScalaString: String =
    if str.contains("\n") || str.contains("\r") || str.contains("\"") then s"\"\"\"$str\"\"\""
    else "\"" + str + "\""

  def STATUS(): Unit = if LOGLEVEL > 0 then println(s"STAT  :  $str")
  def FATAL(): Unit = if LOGLEVEL > 1 then println(s"FATAL: $str")
  def ERROR(): Unit = if LOGLEVEL > 2 then println(s"ERROR: $str")
  def WARN(): Unit = if LOGLEVEL > 3 then println(s"WARN:  $str")
  def INFO(): Unit = if LOGLEVEL > 4 then println(s"INFO:  $str")
  def DEBUG(): Unit = if LOGLEVEL > 5 then println(s"DEBUG: $str")
  def TRACE(): Unit = if LOGLEVEL > 6 then println(s"TRACE: $str")

  def writeTo(fileName: String): Unit = new java.io.PrintWriter(fileName) {
    write(str)
    close()
  }

  def hint(fill: String): String =
    val l = (78 - str.length) / fill.length / 2
    fill * l + " " + str + " " + fill * l

  def hint: String = hint("*")


trait Grid:
  def rel(dx: Int, dy: Int) = ???

/**
 * ensure at least two different nodes
 *
 * @param e1
 * @param e2
 * @param more
 * @tparam T
 * @return
 */
def unique2[T](e1: T, e2: T, more: T*): Set[T] = twoOrMore(ensureNoDuplicates(Seq(e1, e2) ++ more))
def unique2Seq[T](e1: T, e2: T, more: T*): Seq[T] = twoOrMore(ensureNoDuplicates(Seq(e1, e2) ++ more)).toSeq

def unique[T](n: T*): Set[T] = ensureNoDuplicates(n)
