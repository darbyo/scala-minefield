package models

case class Coordinate(x: Int, y: Int)

case class FieldItem(coordinate: Coordinate, isMine: Boolean, mineCount: Int = 0)

case class FieldInformation(info: List[FieldItem])
