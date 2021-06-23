package code_of_the_rings

import scala.collection.mutable
import scala.io.StdIn.readLine

object CodeOfTheRings {

  object Player extends App {
    val magicPhrase = readLine

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    val runes: mutable.Map[Int, Seq[Int]] = mutable.Map.empty
    val initialRuneSet = (1 to 30).foldLeft[Seq[Int]](
      Seq.empty
    )(
      (acc, i) => acc.:+(i)
    )
    runes.put(0, initialRuneSet)

    System.err.println(runes(0).size)

    val symbols = magicPhrase.toCharArray.map {
      case char: Char if char >= 'A' && char <= 'Z' => char.toInt - 'A'.toInt + 1
      case ' ' => 0
    }

    val result: (String, Int) = symbols
      .foldLeft(
        ("", 1)
      )(
        (a, b) => {
          val minChange = getMinChange(b, a._2, runes)
          (a._1 + buildString(minChange), minChange._2)
        }
      )

    println("+.>-.")

    // RuneDelta/finalRuneIndex/CharDelta/InitialChar
    type Accumulator = (Int, Int, Int, Int)

    def getMinChange(targetCharValue: Int,
                     currentRuneIndex: Int,
                     runes: mutable.Map[Int, Seq[Int]]): Accumulator = {
      val minChange: Option[Accumulator] = runes.foldLeft[Option[Accumulator]](
        None
      )(
        (acc: Option[Accumulator], entry) => {
          val charChange = {
            val forwardDir = targetCharValue - entry._1
            val backwardDir = if (entry._1 < targetCharValue) {
              -(entry._1 + (26 - targetCharValue) + 1)
            } else {
              (26 - entry._1) + targetCharValue + 1
            }

            if (forwardDir.abs < backwardDir.abs) {
              forwardDir
            } else {
              backwardDir
            }
          }

          val runeChangeWithIndex =
            entry._2
              .foldLeft[Option[(Int, Int)]](
                None
              ) {
                (acc, rune) =>
                  val forwardDir = rune - currentRuneIndex
                  val backwardDir = if (rune > currentRuneIndex) {
                    -(currentRuneIndex + (30 - rune))
                  } else {
                    (30 - currentRuneIndex) + rune
                  }

                  val minDelta = if (forwardDir.abs > backwardDir.abs) {
                    forwardDir
                  } else {
                    backwardDir
                  }

                  acc.fold(
                    Some(minDelta, rune)
                  )(
                    accVal => if (accVal._1.abs < minDelta) {
                      Some(accVal)
                    } else {
                      Some((minDelta, rune))
                    }
                  )
              }


          val runeChange = runeChangeWithIndex.map(_._1).getOrElse(0)
          val finalRune = runeChangeWithIndex.map(_._2).getOrElse(0)


          acc.fold[Option[Accumulator]](
            Some((runeChange, finalRune, charChange, entry._1))
          )(
            a => if ((a._1.abs + a._3.abs) < (runeChange.abs + charChange.abs)) {
              Some(a)
            } else {
              Some((runeChange, finalRune, charChange, entry._1))
            }
          )
        }
      )

      val runeChange = minChange.map(_._1).get
      val charChange = minChange.map(_._3).get
      val finalRune = minChange.map(_._2).get
      val initialRuneValue = minChange.map(_._4).get


      if (runeChange.abs + charChange.abs > 0) {
        runes.updateWith(initialRuneValue)(
          existing => existing
            .flatMap { seq =>
              val finalSeq = seq.filter(rune => rune != finalRune)
              if (finalSeq.isEmpty) {
                None
              } else {
                Some(finalSeq)
              }
            }
        )
        runes.updateWith(targetCharValue)(a => a.fold(
          Some(Seq(finalRune))
        )(
          a => Some(a.:+(finalRune))
        ))
      }
      minChange.get
    }

    def buildString(minChange: Accumulator): String = {
      val runeChangeString = if (minChange._1 > 0) {
        ">".*(minChange._1)
      } else if (minChange._1 < 0) {
        "<".*(-(minChange._1))
      } else {
        ""
      }

      val charChangeString = if (minChange._3 > 0) {
        "+".*(minChange._3)
      } else if (minChange._3 < 0) {
        "-".*(-minChange._3)
      } else {
        ""
      }
      runeChangeString + charChangeString + "."
    }
  }


}
