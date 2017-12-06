package controllers

import models.{Coordinate, FieldItem}
import org.scalatest.{Matchers, WordSpec}

class MinefieldSpec extends WordSpec with Matchers {

  private def fieldItem(x: Int, y: Int, isMine: Boolean = false) = FieldItem(Coordinate(x, y), isMine)

  "The Minefield object" should {
    "do stuff" in {

    }
  }

  "toCharList" should {
    "return a list of chars" in {
      Minefield.toCharList("...*") shouldBe List('.', '.', '.', '*')
    }

    "throw exception if strings contains any char that is not . or *" in {
      intercept[IllegalArgumentException](
        Minefield.toCharList("q....")
      )
    }
  }

  "createFieldItems" should {
    "without mines" when {
      "return a field item when one is entered" in {
        val expectedFieldItem = FieldItem(Coordinate(1, 1), false)
        val result = Minefield.createFieldItems(List('.'))

        result shouldBe List(expectedFieldItem)
      }

      "return two field items when two are entered" in {
        val expectedFieldItem = FieldItem(Coordinate(1, 1), false)
        val expectedFieldItem2 = FieldItem(Coordinate(1, 2), false)
        val result = Minefield.createFieldItems(List('.', '.'))

        result shouldBe List(expectedFieldItem, expectedFieldItem2)
      }

      "return field item List for 1 row with correct coordinates" in {
        val result = Minefield.createFieldItems(List('.', '.', '.', '.'))

        result shouldBe List(fieldItem(1,1),fieldItem(1,2),fieldItem(1,3),fieldItem(1,4))
      }

      "return field item List for 2 rows with correct coordinates" in {
        val result = Minefield.createFieldItems(List('.', '.', '.', '.', '.', '.', '.', '.'))

        result shouldBe List(
          fieldItem(1,1),fieldItem(1,2),fieldItem(1,3),fieldItem(1,4),
          fieldItem(2,1),fieldItem(2,2),fieldItem(2,3),fieldItem(2,4)
        )
      }

      "return field item List for 3 rows with correct coordinates" in {
        val result = Minefield.createFieldItems(List('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'))

        result shouldBe List(
          fieldItem(1,1),fieldItem(1,2),fieldItem(1,3),fieldItem(1,4),
          fieldItem(2,1),fieldItem(2,2),fieldItem(2,3),fieldItem(2,4),
          fieldItem(3,1),fieldItem(3,2),fieldItem(3,3),fieldItem(3,4)
        )
      }

      "return field item List for 4 rows with correct coordinates" in {
        val result = Minefield.createFieldItems(List('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.'))

        result shouldBe List(
          fieldItem(1,1),fieldItem(1,2),fieldItem(1,3),fieldItem(1,4),
          fieldItem(2,1),fieldItem(2,2),fieldItem(2,3),fieldItem(2,4),
          fieldItem(3,1),fieldItem(3,2),fieldItem(3,3),fieldItem(3,4),
          fieldItem(4,1),fieldItem(4,2),fieldItem(4,3),fieldItem(4,4)
        )
      }
    }

    "with mines" when {
      "return with isMine false when no mine is entered" in {
        val result = Minefield.createFieldItems(List('.'))

        result shouldBe List(fieldItem(x = 1, y = 1, isMine = false))
      }

      "return with isMine true when * is entered" in {
        val result = Minefield.createFieldItems(List('*'))

        result shouldBe List(fieldItem(x = 1, y = 1, isMine = true))
      }

      "return with isMine true when * is entered as second item" in {
        val result = Minefield.createFieldItems(List('.', '*'))

        result shouldBe List(
          fieldItem(x = 1, y = 1, isMine = false),
          fieldItem(1, 2, isMine = true)
        )
      }

      "return with isMine true when * is entered in second row" in {
        val result = Minefield.createFieldItems(List('.', '.', '.', '.', '*', '.', '.', '.'))

        result shouldBe List(
          fieldItem(1,1), fieldItem(1,2), fieldItem(1,3), fieldItem(1, 4),
          fieldItem(2,1, isMine = true), fieldItem(2,2), fieldItem(2,3), fieldItem(2, 4)
        )
      }

      "return with isMine true when * is entered in third row" in {
        val result = Minefield.createFieldItems(List('.', '.', '.', '.', '.', '.', '.', '.', '*', '.', '.', '.'))

        result shouldBe List(
          fieldItem(1,1), fieldItem(1,2), fieldItem(1,3), fieldItem(1, 4),
          fieldItem(2,1), fieldItem(2,2), fieldItem(2,3), fieldItem(2, 4),
          fieldItem(3,1, isMine = true), fieldItem(3,2), fieldItem(3,3), fieldItem(3, 4)
        )
      }

      "return with isMine true when * is entered in fourth row" in {
        val result = Minefield.createFieldItems(List('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '*', '.', '.', '.'))

        result shouldBe List(
          fieldItem(1,1), fieldItem(1,2), fieldItem(1,3), fieldItem(1, 4),
          fieldItem(2,1), fieldItem(2,2), fieldItem(2,3), fieldItem(2, 4),
          fieldItem(3,1), fieldItem(3,2), fieldItem(3,3), fieldItem(3, 4),
          fieldItem(4,1, isMine = true), fieldItem(4,2), fieldItem(4,3), fieldItem(4, 4)
        )
      }

      "return with isMine true when * is entered in 3,4 row" in {
        val result = Minefield.createFieldItems(List('.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '*', '.', '.', '.', '.'))

        result shouldBe List(
          fieldItem(1,1), fieldItem(1,2), fieldItem(1,3), fieldItem(1, 4),
          fieldItem(2,1), fieldItem(2,2), fieldItem(2,3), fieldItem(2, 4),
          fieldItem(3,1), fieldItem(3,2), fieldItem(3,3), fieldItem(3, 4, isMine = true),
          fieldItem(4,1), fieldItem(4,2), fieldItem(4,3), fieldItem(4, 4)
        )
      }
    }
  }

  "getFieldsToIncrement" should {
    "return correct fields to increment for one row location 1,1" in {
      val result = Minefield.getFieldsToIncrement(
        List(fieldItem(1, 1, true), fieldItem(1, 2), fieldItem(1, 3), fieldItem(1, 4)),
        List(fieldItem(1, 1, true))
      )

      result shouldBe List(fieldItem(1, 2))
    }

    "return correct fields to increment for one row location 1,2" in {
      val result = Minefield.getFieldsToIncrement(
        List(fieldItem(1, 1), fieldItem(1, 2, true), fieldItem(1, 3), fieldItem(1, 4)),
        List(fieldItem(1, 2, true))
      )

      result shouldBe List(fieldItem(1, 1), fieldItem(1, 3))
    }

    "return correct fields to increment for one row 1,3" in {
      val result = Minefield.getFieldsToIncrement(
        List(fieldItem(1, 1), fieldItem(1, 2), fieldItem(1, 3, true), fieldItem(1, 4)),
        List(fieldItem(1, 3, true))
      )

      result shouldBe List(fieldItem(1, 2), fieldItem(1, 4))
    }

    "return correct fields to increment for one row location 1,4" in {
      val result = Minefield.getFieldsToIncrement(
        List(fieldItem(1, 1), fieldItem(1, 2), fieldItem(1, 3), fieldItem(1, 4, true)),
        List(fieldItem(1, 4, true))
      )

      result shouldBe List(fieldItem(1, 3))
    }

    "return correct fields to increment for two rows location 2,1" in {
      val result = Minefield.getFieldsToIncrement(
        List(
          fieldItem(1, 1), fieldItem(1, 2), fieldItem(1, 3), fieldItem(1, 4),
          fieldItem(2, 1, true), fieldItem(2, 2), fieldItem(2, 3), fieldItem(2, 4)
        ),
        List(fieldItem(2, 1, true))
      )

      result shouldBe List(fieldItem(1, 1), fieldItem(1, 2), fieldItem(2, 2))
    }

    "return correct fields to increment for two rows location 2,3" in {
      val result = Minefield.getFieldsToIncrement(
        List(
          fieldItem(1, 1), fieldItem(1, 2), fieldItem(1, 3), fieldItem(1, 4),
          fieldItem(2, 1), fieldItem(2, 2), fieldItem(2, 3, true), fieldItem(2, 4)
        ),
        List(fieldItem(2, 3, true))
      )

      result shouldBe List(fieldItem(2, 2), fieldItem(1, 2), fieldItem(1, 3), fieldItem(1, 4), fieldItem(2,4))
    }

    "return correct fields to increment for three rows location 3,1" in {
      val result = Minefield.getFieldsToIncrement(
        List(
          fieldItem(1, 1), fieldItem(1, 2), fieldItem(1, 3), fieldItem(1, 4),
          fieldItem(2, 1), fieldItem(2, 2), fieldItem(2, 3), fieldItem(2, 4),
          fieldItem(3, 1), fieldItem(3, 2), fieldItem(3, 3, true), fieldItem(3, 4)
        ),
        List(fieldItem(3, 1, true))
      )

      result shouldBe List(fieldItem(2, 1), fieldItem(2, 2), fieldItem(3, 2))
    }

    "return correct fields to increment for four rows location 4,2" in {
      val result = Minefield.getFieldsToIncrement(
        List(
          fieldItem(1, 1), fieldItem(1, 2), fieldItem(1, 3), fieldItem(1, 4),
          fieldItem(2, 1), fieldItem(2, 2), fieldItem(2, 3), fieldItem(2, 4),
          fieldItem(3, 1), fieldItem(3, 2), fieldItem(3, 3), fieldItem(3, 4),
          fieldItem(4, 1), fieldItem(4, 2), fieldItem(4, 3, true), fieldItem(4, 4)
        ),
        List(fieldItem(4, 3, true))
      )

      result shouldBe List(fieldItem(4, 2), fieldItem(3, 2), fieldItem(3, 3), fieldItem(3, 4), fieldItem(4,4))
    }

    "return correct fields to increment for four rows where bomb is in the middle" in {
      val result = Minefield.getFieldsToIncrement(
        List(
          fieldItem(1, 1), fieldItem(1, 2), fieldItem(1, 3), fieldItem(1, 4),
          fieldItem(2, 1), fieldItem(2, 2), fieldItem(2, 3), fieldItem(2, 4),
          fieldItem(3, 1), fieldItem(3, 2, true), fieldItem(3, 3), fieldItem(3, 4),
          fieldItem(4, 1), fieldItem(4, 2), fieldItem(4, 3), fieldItem(4, 4)
        ),
        List(fieldItem(3, 2, true))
      )

      result shouldBe List(fieldItem(4, 3), fieldItem(4, 2), fieldItem(4,1), fieldItem(3, 1), fieldItem(2, 1), fieldItem(2, 2), fieldItem(2, 3), fieldItem(3, 3))
    }
  }

  "incrementMinefield" should {
    "increment fieldItems for one rowz" in {
      val result = Minefield.incrementMinefield(
        List(fieldItem(1, 1), fieldItem(1, 2, true), fieldItem(1, 3), fieldItem(1, 4)),
        List(fieldItem(1,1), fieldItem(1,3))
      )

      result shouldBe "1*10"
    }

    "increment fieldItems for one row mine location 1,3" in {
      val result = Minefield.incrementMinefield(
        List(fieldItem(1, 1), fieldItem(1, 2), fieldItem(1, 3, true), fieldItem(1, 4)),
        List(fieldItem(1,2), fieldItem(1,4))
      )

      result shouldBe "01*1"
    }

    "increment fieldItems for two rows" in {
      val result = Minefield.incrementMinefield(
        mineField = List(
          fieldItem(1, 1), fieldItem(1, 2), fieldItem(1, 3, true), fieldItem(1, 4),
          fieldItem(2, 1), fieldItem(2, 2), fieldItem(2, 3, true), fieldItem(2, 4)
        ),
        fieldsToIncrement = List(fieldItem(1,2), fieldItem(1,4), fieldItem(2,2), fieldItem(2,4))
      )

      result shouldBe "01*1"
    }
  }
}