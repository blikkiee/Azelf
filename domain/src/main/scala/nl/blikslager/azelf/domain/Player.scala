package nl.blikslager.azelf.domain

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
                    new Player(floorLine.fill(tiles), patternLines, wall, score)
                }
                else {
                    val (patternLine: PatternLine, remainingTiles: List[Tile]) = p.fill(tiles)
                    val updatedPatternLines: List[PatternLine] = patternLines.updated(selectedPatternLine-1, patternLine)
                    new Player(floorLine.fill(remainingTiles), updatedPatternLines, wall, score)
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
        val wallScore: Int = updatedWall.getScore
        val (floorLineScore: Int, floorLineTiles: List[Tile], updatedFloorLine: FloorLine) = floorLine.score
        val newScore: Int = floorLineScore + wallScore + score
        (new Player(updatedFloorLine, updatedPatternLines, updatedWall, newScore), redundantTiles :++ floorLineTiles)
    }

    override def toString(): String = {
        s"${for(pl <- patternLines) yield s"${pl.toString()}\n"}${floorLine}\n${wall}"
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
    override def toString(): String = {
        if (filledSpaces == 0){
             (for(i <- 1 to spaces) yield "_").mkString("PatternLine[", " ", "]")
        }
        else if (filledSpaces == spaces){
            (for(i <- 1 to spaces) yield "x").mkString("PatternLine[", " ", s"][${tileColour.get}]")
        }
        else {
            (for(i <- 1 to spaces - filledSpaces) yield "_").mkString(
                (for(i <- 1 to filledSpaces) yield "x").mkString("PatternLine["," ", " "),
                " ",
                s"][${tileColour.get}]"
            )
        }
    }
}

class FloorLine(
    val tiles: List[Tile] = List()
){
    def length: Int = tiles.length
    def fill(newTiles: List[Tile]): FloorLine = new FloorLine(tiles :++ newTiles)
    def peekScore: Int = if(length < 3) length * -1 else if (length < 6) length * -2 + 2 else length * -3 + 7
    def score: (Int, List[Tile], FloorLine) = (peekScore, tiles, new FloorLine)
    override def toString(): String = "FloorLine[" + tiles.mkString(", ") + "][score: " + peekScore + "]"
    override def equals(other: Any): Boolean = other match {
        case that: FloorLine => this.tiles == that.tiles
        case _ => false
    }
}

object Wall {
    def apply(): Wall = {
        @tailrec def rearrange(list: List[WallTile], iterations: Int) : List[WallTile] = if(iterations == 0) list else rearrange(list.last +: list.dropRight(1), iterations - 1)
        val row: List[WallTile] = List(WallTile(Blue), WallTile(Yellow), WallTile(Red), WallTile(Purple), WallTile(Green))
        val matrix: List[List[WallTile]] = (for (iRow <- 0 to 4) yield rearrange(row, iRow)).toList
        new Wall(matrix)
    }
}

class Wall private (
    private val tiles: List[List[WallTile]]
){
    def placeTile(tile: Tile, row: Int): Wall = {
        val selectedRow: Option[List[WallTile]] = tiles.lift(row-1)
        selectedRow match {
            case Some(selectedWallRow) => {
                val tilePositionInRow: Int = selectedWallRow.indexWhere({case WallTile(colour, _) => colour == tile})
                val updatedRow: List[WallTile] = selectedWallRow.updated(tilePositionInRow, WallTile(tile, true))
                new Wall(tiles.updated(row-1, updatedRow))
            }
            case None => this
        }
    }

    def getTile(row: Int, column: Int): Option[WallTile] = for {
            col <- tiles.lift(row - 1)
            tile <- col.lift(column - 1) 
        } yield tile

    def countTiles: Int = tiles.flatten.filter({case WallTile(_, isFilled) => isFilled }).length

    def rowContains(rowIndex: Int, tileColour: Tile) = {
        tiles(rowIndex-1).exists(t => t == WallTile(tileColour, true))
    }

    def getScore: Int = countTiles + countDoubleValueTiles

    def countDoubleValueTiles: Int = {
        def hasHorizontalNeighbour(row: Int, column: Int): Boolean =  {
            val r = tiles(row) // lift?
            column < 4 && r(column+1).isFilled || 
            column > 0 && r(column-1).isFilled
        }
        def hasVerticalNeighbour(row: Int, column: Int): Boolean = {
            row < 4 && tiles(row+1)(column).isFilled ||             
            row > 0 && tiles(row-1)(column).isFilled
        }
        def isDoubleValueTile(row: Int, column: Int): Boolean = {
            tiles(row)(column).isFilled &&
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
        def rowIsComplete(row: Int) : Boolean = tiles(row).filter(x => x.isFilled).length == 5
        def columnIsComplete(column: Int) : Boolean = (for(row <- tiles; if(row(column).isFilled)) yield true).toList.length == 5 
        def diagonalIsComplete(tile: Tile) : Boolean = (for(row <- tiles; if(row.exists(x => x.colour == tile && x.isFilled))) yield true).toList.length == 5
        val rowsScore: Int = (0 to 4).fold(0){ (acc: Int, row: Int) => if(rowIsComplete(row)) acc + 2 else acc }
        val columnsScore: Int = (0 to 4).fold(0){ (acc: Int, column: Int) => if(columnIsComplete(column)) acc + 7 else acc }
        val diagonalsScore: Int = List(Purple, Blue, Green, Red, Yellow).foldLeft(0){ (acc: Int, tile: Tile) => if(diagonalIsComplete(tile)) acc + 10 else acc }

        rowsScore + columnsScore + diagonalsScore
    }

    override def toString(): String = {
        def rowToString(row: List[WallTile]) : String = 
            (for(tile <- row) yield {
                if(tile.isFilled) tile.colour.toString()(0) 
                else "_" 
            }).mkString("|", " ", "|")

        (for(row <- tiles) yield rowToString(row)).mkString("\n")
    }
}

object WallTile {
    def apply(colour: Tile, isFilled: Boolean = false): WallTile = new WallTile(colour, isFilled)
}
case class WallTile(val colour: Tile, val isFilled: Boolean = false)