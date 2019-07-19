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
    val wall: Wall
){
    def placeTilesOnPatternLine(tiles: List[Tile], selectedPatternLine: Int) : Player = {
        patternLines.find(x => x.spaces == selectedPatternLine) match {
            case Some(p) => {
                val (patternLine: PatternLine, remainingTiles: List[Tile]) = p.fill(tiles)
                val updatedPatternLines: List[PatternLine] = patternLines.updated(selectedPatternLine-1, patternLine)
                new Player(floorLine.fill(remainingTiles), updatedPatternLines, wall)
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
        // todo: update floorLine
        (new Player(floorLine, updatedPatternLines, updatedWall), redundantTiles)
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
        val selectedRow: List[(Tile, Boolean)] = tiles(row-1) //lift
        val rowIndex: Int = selectedRow.indexOf((tile, false))
        val newRow: List[(Tile, Boolean)] = selectedRow.updated(rowIndex, (tile, true))
        new Wall(tiles.updated(row-1, newRow))
    }

    def getTile(row: Int, column: Int): Option[(Tile, Boolean)] = for {
            col <- tiles.lift(row - 1)
            tile <- col.lift(column - 1) 
        } yield tile

    def countTiles: Int = tiles.flatten.filter({case (_, b) => b }).length
}