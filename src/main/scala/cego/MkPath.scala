package cego

import scala.annotation.tailrec

/**
 *
 * @param depth numbers to process at the end (> 0)
 * @param post  char to fill path snippets post.length == 1
 * @param pre  char to fill path snippets
 */
class MkPath private(val depth: Int, val post: String, val pre: String):
  def dir(str: String): List[String] =
    val revNum = str.trim.reverse.takeWhile(_.isDigit)
    val digits = revNum.take(depth).reverse
    if digits.isEmpty then throw IllegalArgumentException("must end with \\d")
    val padded = if digits.length >= depth then digits else "0" * (depth - digits.length) + digits

    @tailrec
    def recurse(acu: List[String], n: Int): List[String] =
      if n == depth then acu.reverse
      else
        val inc = n + 1
        val tmp = pre + padded.take(inc) + post * (depth - inc)
        recurse(tmp :: acu, inc)

    revNum.reverse :: recurse(Nil, 0)

object MkPath:
  def apply(depth: Int = 2, post: String = "_", pre: String = ""): MkPath =
    require(post.length == 1)
    require(depth > 0)
    new MkPath(depth, post, pre)
