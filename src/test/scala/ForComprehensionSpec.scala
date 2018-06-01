//Aufbau analog zu http://www.scalatest.org/quick_start

import org.scalatest.{FlatSpec, Matchers}

/** Testtreiber zur Demo von Fähigkeiten der For-Comprehension wie in Kap. 21 des Piepmeyer-Buchs.
  * Siehe auch https://docs.scala-lang.org/tour/for-comprehensions.html */
class ForComprehensionSpec extends FlatSpec with Matchers {

  //Kapitel 21.1 Nicht so funktionale Methode höherer Ordnung

  "for-generator" should "handle each element like foreach" in {
    //Wir bilden eine List aus Pizza-Zutaten:
    val zutaten = List("Hefe", "Wasser", "Mehl");
    {
      val out = new StringBuilder
      // Alle Pizza-Zutaten behandeln können wir mit der Methode höherer Ordnung foreach:
      zutaten foreach out.append
      out.toString shouldBe "HefeWasserMehl"
    }
    {
      val out = new StringBuilder
      // Die for-Loop bietet uns dafür imperativ aussehende Syntax:
      for (zutat <- zutaten) { //Lesen: Für jede zutat in zutaten.
        // zutat<-zutaten ist dabei ein Generator.
        out ++= zutat
      }
      out.toString shouldBe "HefeWasserMehl"
    }
  }

  "multiple Generators" should "combine each of their elements" in { //Kap. 21.2
    val out = new StringBuilder
    // Alle Feldnamen eines Schachbretts generieren:
    for (buchstabe <- 'A' to 'H'; zahl <- 1 to 8) {
      out ++= s"$buchstabe$zahl "
    }
    out.toString shouldBe "A1 A2 A3 A4 A5 A6 A7 A8 B1 B2 B3 B4 B5 B6 B7 B8 C1 C2 C3 C4 C5 C6 C7 C8 D1 D2 D3 D4 D5 D6 D7 D8 E1 E2 E3 E4 E5 E6 E7 E8 F1 F2 F3 F4 F5 F6 F7 F8 G1 G2 G3 G4 G5 G6 G7 G8 H1 H2 H3 H4 H5 H6 H7 H8 "
  }

  "multiple Generators on separate lines" should "combine each of their elements" in { //Kap. 21.2
    val out = new StringBuilder
    // Alle Feldnamen eines Schachbretts generieren.
    //Mit geschweiften statt runden Klammern um die Generator-Folge. 
    //Dann darf das Semikolon durch Zeilenwechsel ersetzt werden:
    for {
      buchstabe <- 'A' to 'H'
      zahl <- 1 to 8
    } {
      out ++= s"$buchstabe$zahl "
    }
    out.toString shouldBe "A1 A2 A3 A4 A5 A6 A7 A8 B1 B2 B3 B4 B5 B6 B7 B8 C1 C2 C3 C4 C5 C6 C7 C8 D1 D2 D3 D4 D5 D6 D7 D8 E1 E2 E3 E4 E5 E6 E7 E8 F1 F2 F3 F4 F5 F6 F7 F8 G1 G2 G3 G4 G5 G6 G7 G8 H1 H2 H3 H4 H5 H6 H7 H8 "
  }

  "for with if" should "filter matching elements" in { //Kap. 21.3
    val zutaten = List("Hefe", "Wasser", "Mehl")
    val out = new StringBuilder
    // Alle Zutaten mit 'H' oder 'h':
    for (
      zutat <- zutaten
      if zutat.toUpperCase.contains('H')
    ) {
      out ++= zutat
    }
    out.toString shouldBe "HefeMehl"
  }

  "for with =" should "map elements" in { //Kap. 21.3
    val zutaten = List("Hefe", "Wasser", "Mehl")
    val out = new StringBuilder
    // Alle Zutaten in Großbuchstaben:
    for {
      zutat <- zutaten //generator
      mapped = zutat.toUpperCase //value definition maps
    } {
      out ++= mapped ++= " "
    }
    out.toString shouldBe "HEFE WASSER MEHL "
  }

  it should "flatMap elements" in { //Kap. 21.3
    val zutaten = List("Hefe", "Wasser", "Mehl")
    val out = new StringBuilder
    // Mache aus geschachtelten Collections eine flache Collection:
    for {
      zutat <- zutaten //generator over List of Strings
      zu = zutat.toUpperCase //value definition maps
      c <- zu //generator over characters in String
    } {
      out ++= c + " "
    }
    out.toString shouldBe "H E F E W A S S E R M E H L "
  }

  "for with yield" should "map elements and return them" in { //Kap. 21.4 for-Comprehension
    val zutaten = List("Hefe", "Wasser", "Mehl")
    // Liste aller Zutaten in Großbuchstaben:
    val result = for (zutat <- zutaten) yield zutat.toUpperCase
    result shouldBe List("HEFE", "WASSER", "MEHL")
  }

  it should "flatMap elements" in { //Kap. 21.4
    //Alle Feldnamen eines Schachbretts als Liste:
    val result = for {
      buchstabe <- 'A' to 'H'
      zahl <- 1 to 8
    } yield s"$buchstabe$zahl"
    result shouldBe List("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8")
  }

  "nested maps and flatten" should "do the same" in { //Kap. 21.4
    //Alle Feldnamen eines Schachbretts als Liste:
    val result = ('A' to 'H') map {
      buchstabe =>
        (1 to 8) map {
          zahl => s"$buchstabe$zahl"
        }
    }
    result.flatten shouldBe List("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8")
  }

  "for with yield" should "flatMap heterogenous collections" in { //Kap. 21.4
    //Auch die max. einelementige Option ist eine Collection:
    val personOptions: List[Option[String]] = List(Some("Willi"), None, Some("Mike"))
    val result = for (
      pO <- personOptions;
      p <- pO
    ) yield p
    result shouldBe List("Willi", "Mike")
  }

  /** This method can be used as a placeholder for code which should return something
    * which should be matched.
    * For example you could write ' XXX shouldBe "Nirvana" ' if you want to compile the test
    * and replace XXX later by an expression, which returns "Nirvana".
    *
    * @throws NotImplementedError always */
  private def XXX = convertToAnyShouldWrapper {
    throw new NotImplementedError("Code-to-test still missing. See 2nd line of stack trace.")
  }

}
