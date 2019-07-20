package Azelf

import scala.annotation.tailrec

object Player {
    def apply(): Player = {
        val patternLines = (for(i <- 1 to 5) yield new PatternLine(i)).toList
        new Player(new FloorLine, patternLines, Wall())
    }
}

class Player private(
    val floorLine: FloorLine, 
    val patternLines: List[PatternLine],
    val wall: Wall,
    val score: Int = 0
){
    def placeTilesOnPatternLine(tiles: List[Tile], selectedPatternLine: Int) : Player = {
        patternLines.find(x => x.spaces == selectedPatternLine) match {
            case Some(p) => {
                val wallRowIndex = patternLines.indexOf(p) + 1
                if(wall.rowContains(wallRowIndex, tiles.head)) {
                    new Player(floorLine.fill(tiles), patternLines, wall)
                }
                else {
                    val (patternLine: PatternLine, remainingTiles: List[Tile]) = p.fill(tiles)
                    val updatedPatternLines: List[PatternLine] = patternLines.updated(selectedPatternLine-1, patternLine)
                    new Player(floorLine.fill(remainingTiles), updatedPatternLines, wall)
                }
            }
            case None => this
        }
    }
    def updateScore: (Player, List[Tile]) = {
        @tailrec
        def coverWall(patternLinesToUpdate: List[PatternLine], currentPatternLines: List[PatternLine], currentWall: Wall, currentTiles: List[Tile] = List()): (List[PatternLine], Wall, List[Tile]) = {
            val currentPL: PatternLine = patternLinesToUpdate.head
            val indexCurrentPL: Int = currentPatternLines.indexOf(currentPL)
            val (newPL: PatternLine, plTiles: List[Tile]) = currentPL.empty
            val (newWall: Wall, redundantPLTiles: List[Tile]) = placeOnWall(currentWall, plTiles, indexCurrentPL + 1)
            val newPatternLines: List[PatternLine] = currentPatternLines.updated(indexCurrentPL, newPL)
            if(patternLinesToUpdate.tail.isEmpty) (newPatternLines, newWall, redundantPLTiles) else coverWall(patternLinesToUpdate.tail, newPatternLines, newWall, redundantPLTiles)
        }
        def placeOnWall(w: Wall, t: List[Tile], row: Int): (Wall, List[Tile]) = {
            (w.placeTile(t.head, row), t.tail)
        }
        
        val completedPatternLines = patternLines.filter(pl => pl.isComplete)
        val (updatedPatternLines: List[PatternLine], updatedWall: Wall, redundantTiles: List[Tile]) = coverWall(completedPatternLines, patternLines, wall)
        val wallScore: Int = 0 // updatedWall.getScore
        val (floorLineScore: Int, floorLineTiles: List[Tile], updatedFloorLine: FloorLine) = floorLine.score
        (new Player(updatedFloorLine, updatedPatternLines, updatedWall, floorLineScore + wallScore), redundantTiles :++ floorLineTiles)
    }
}

class PatternLine(
    val spaces: Int, 
    val filledSpaces: Int = 0, 
    val tileColour: Option[Tile] = None
){
    def isComplete: Boolean = spaces == filledSpaces
    def openSpaces: Int = spaces - filledSpaces
    def fill(tiles: List[Tile]): (PatternLine, List[Tile]) = {
        def fillAndUpdate(): (PatternLine, List[Tile]) = {
            val newFilledSpaces: Int = if (tiles.length > openSpaces) spaces else tiles.length + filledSpaces
            val newTileColour: Option[Tile] = if (filledSpaces == 0) Option(tiles.head) else tileColour
            val remainingTiles: List[Tile] = tiles.drop(newFilledSpaces-filledSpaces)
            (new PatternLine(spaces, newFilledSpaces, newTileColour), remainingTiles)
        }
        if(filledSpaces == 0 || Option(tiles.head) == tileColour) fillAndUpdate else (this, tiles)
    }
    def empty: (PatternLine, List[Tile]) = if(isComplete) (new PatternLine(spaces), Azul.createTileCollection(List((tileColour.get, spaces)))) else (this, List())
}

class FloorLine(
    val tiles: List[Tile] = List()
){
    def length: Int = tiles.length
    def fill(newTiles: List[Tile]): FloorLine = new FloorLine(tiles :++ newTiles)
    def peekScore: Int = if(length < 3) length * -1 else if (length < 6) length * -2 + 2 else length * -3 + 7
    def score: (Int, List[Tile], FloorLine) = (peekScore, tiles, new FloorLine)
    override def equals(other: Any): Boolean = other match {
        case that: FloorLine => this.tiles == that.tiles
        case _ => false
    }
}

object Wall {
    def apply(): Wall = {
        @tailrec def rearrange(list: List[(Tile, Boolean)], iterations: Int) : List[(Tile, Boolean)] = if(iterations == 0) list else rearrange(list.last +: list.dropRight(1), iterations - 1)
        val row: List[(Tile, Boolean)] = List((Blue, false), (Yellow, false), (Red, false), (Black, false), (Green, false))
        val matrix: List[List[(Tile, Boolean)]] = (for (iRow <- 0 to 4) yield rearrange(row, iRow)).toList
        new Wall(matrix)
    }
}

class Wall private (
    private val tiles: List[List[(Tile, Boolean)]]
){
    def placeTile(tile: Tile, row: Int): Wall = {
        val selectedRow: List[(Tile, Boolean)] = tiles(row-1) // todo: lift
        val rowIndex: Int = selectedRow.indexWhere({case (colour, _) => colour == tile})
        val newRow: List[(Tile, Boolean)] = selectedRow.updated(rowIndex, (tile, true))
        new Wall(tiles.updated(row-1, newRow))
    }

    def getTile(row: Int, column: Int): Option[(Tile, Boolean)] = for {
            col <- tiles.lift(row - 1)
            tile <- col.lift(column - 1) 
        } yield tile

    def countTiles: Int = tiles.flatten.filter({case (_, b) => b }).length

    def rowContains(rowIndex: Int, tileColour: Tile) = {
        tiles(rowIndex-1).exists(t => t == (tileColour, true))
    }

    def getScore: Int = countTiles + countDoubleValueTiles

    def countDoubleValueTiles: Int = {
        def hasHorizontalNeighbour(row: Int, column: Int): Boolean =  {
            val r = tiles(row) // lift?
            column < 4 && r(column+1)._2 || 
            column > 0 && r(column-1)._2
        }
        def hasVerticalNeighbour(row: Int, column: Int): Boolean = {
            row < 4 && tiles(row+1)(column)._2 ||             
            row > 0 && tiles(row-1)(column)._2
        }
        def isDoubleValueTile(row: Int, column: Int): Boolean = {
            tiles(row)(column)._2 &&
            hasHorizontalNeighbour(row, column) && 
            hasVerticalNeighbour(row, column)
        }

        (for {
            row <- 0 to 4
            col <- 0 to 4
            if(isDoubleValueTile(row, col))
        } yield true).length
    }

    def getFinalScore: Int = getScore + getComboScore

    // todo: refactor
    def getComboScore: Int = {
        def rowIsComplete(row: Int) : Boolean = tiles(row).filter(x => x._2).length == 5
        def columnIsComplete(column: Int) : Boolean = (for(row <- tiles; if(row(column)._2)) yield true).toList.length == 5 
        def diagonalIsComplete(tile: Tile) : Boolean = (for(row <- tiles; if(row.exists(x => x._1 == tile && x._2))) yield true).toList.length == 5
        val rowsScore: Int = (0 to 4).fold(0){ (acc: Int, row: Int) => if(rowIsComplete(row)) acc + 2 else acc }
        val columnsScore: Int = (0 to 4).fold(0){ (acc: Int, column: Int) => if(columnIsComplete(column)) acc + 7 else acc }
        val diagonalsScore: Int = List(Black, Blue, Green, Red, Yellow).foldLeft(0){ (acc: Int, tile: Tile) => if(diagonalIsComplete(tile)) acc + 10 else acc }

        rowsScore + columnsScore + diagonalsScore
    }
}