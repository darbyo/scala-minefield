package controllers

import models.{Coordinate, FieldItem}

object Minefield {

  def minefield(field: String) = {
    val charList = toCharList(field)
    val mineField = createFieldItems(charList)
    val mineLocations = getMineLocations(mineField)
    val fieldsToIncrement = getFieldsToIncrement(mineField, mineLocations)
    val incrementedMinefield = incrementMinefield(mineField, fieldsToIncrement)
  }

  private case class IncrementedMinefield(c: Coordinate, isMine: Boolean, increment: Boolean)

  def incrementMinefield(mineField: List[FieldItem], fieldsToIncrement: List[FieldItem]): List[FieldItem] = {
    fieldsToIncrement match {
      case List() => mineField
      case x :: xs => incrementMinefield(incrementField(x.coordinate, mineField), xs)
    }
  }

  private def incrementField(coord: Coordinate, mineField: List[FieldItem]): List[FieldItem] = {
    mineField.find(_.coordinate == coord) match {
      case None => mineField
      case Some(toIncrement) => toIncrement.copy(mineCount = toIncrement.mineCount + 1) :: mineField.filter(_ != toIncrement)
    }
  }

  def createFieldItems(minefield: List[Char]) = {
    val x = for (counter <- 1 to minefield.length) yield {
      val coordinate = getCoordinate(counter)
      val mine = isMine(minefield, counter)

      FieldItem(coordinate, mine)
    }
    x.toList
  }
  
  def getFieldsToIncrement(fieldItems: List[FieldItem], mineLocations: List[FieldItem]) = {
    val neighbourCoordinates: List[Coordinate] = getNeighbours(mineLocations)

    val fieldItemsToIncrement:List[List[FieldItem]] = for(coordinate <- neighbourCoordinates) yield {
      fieldItems.filter(item => item.coordinate.equals(coordinate))
    }
    fieldItemsToIncrement.flatten
  }

  private def getNeighbours(mineLocations: List[FieldItem]) = {
    val coordinateToIncrementList = for (ml <- mineLocations) yield {
      List(
        Coordinate(x = ml.coordinate.x + 1, y = ml.coordinate.y + 1),
        Coordinate(x = ml.coordinate.x + 1, y = ml.coordinate.y),
        Coordinate(x = ml.coordinate.x + 1, y = ml.coordinate.y - 1),
        Coordinate(x = ml.coordinate.x, y = ml.coordinate.y - 1),
        Coordinate(x = ml.coordinate.x - 1, y = ml.coordinate.y - 1),
        Coordinate(x = ml.coordinate.x - 1, y = ml.coordinate.y),
        Coordinate(x = ml.coordinate.x - 1, y = ml.coordinate.y + 1),
        Coordinate(x = ml.coordinate.x, y = ml.coordinate.y + 1)
      )
    }

    coordinateToIncrementList.flatten
  }

  def toCharList(s:String) = {
    if(s.contains(".") && s.contains("*")){
      s.toList
    } else {
      throw new IllegalArgumentException("String contains illegal character")
    }
  }

  private def getMineLocations(fieldItems: List[FieldItem]) = {
    fieldItems.filter(x => x.isMine)
  }

  private def isMine(minefield: List[Char], counter: Int): Boolean = {
    val updatedList = minefield.take(counter)
    val lastItem = updatedList.lastOption

    lastItem.fold[Boolean](false){ x =>
      if(x == '*') true else false
    }
  }

  private def getCoordinate(counter: Int) = {
    val coordinate = counter match {
      case coord if (coord >= 1 && coord <= 4) => Coordinate(1, coord)
      case coord if (coord >= 5 && coord <= 8) => Coordinate(2, if (coord % 4 == 0) 4 else coord % 4)
      case coord if (coord >= 9 && coord <= 12) => Coordinate(3, if (coord % 4 == 0) 4 else coord % 4)
      case coord if (coord >= 12 && coord <= 16) => Coordinate(4, if (coord % 4 == 0) 4 else coord % 4)
      case _ => ???
    }
    coordinate
  }
}