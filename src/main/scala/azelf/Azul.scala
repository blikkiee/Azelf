package Azelf

import scala.util._

// Create collection with tiles (10x black, 10x blue, 10x green, 10x red, 10x yellow) and shuffle it
// Distribute tiles to tilefactory

// todo: refactor; leesbaarder maken van functies: wat (voor stappen) doe je?

object Azul {
    // ? dictionary ipv list van tuples?
    def createTileCollection(specification: List[(Tile, Int)]): List[Tile] = {
        (for ( (colour: Tile, amount: Int) <- specification) yield (for (i <- 1 to amount) yield colour).toList).flatten
    }

    def shuffleTiles(tiles: List[Tile]): List[Tile] = {
        def shuffleRecursive(): List[Tile] = {
            val shuffled = Random.shuffle(tiles)
            if (shuffled == tiles) shuffleRecursive else shuffled
        }
        if (tiles.distinct.length > 1) shuffleRecursive() else tiles
    }
}

object Player{
    def apply(): Player = {
        val patternLines = (for(i <- 1 to 5) yield new PatternLine(i)).toList
        new Player(new FloorLine(), patternLines)
    }
}

class Player private(
    val floorLine: FloorLine, 
    val patternLines: List[PatternLine]
    ){
        def placeTilesOnPatternLine(tiles: List[Tile], selectedPatternLine: Int) : Player = {
            patternLines.find(x => x.spaces == selectedPatternLine) match {
                case Some(p) => {
                    val (patternLine: PatternLine, remainingTiles: List[Tile]) = p.fill(tiles)
                    val updatedPatternLines: List[PatternLine] = patternLines.updated(selectedPatternLine-1, patternLine)
                    new Player(new FloorLine(remainingTiles), updatedPatternLines)
                }
                case None => this
            }
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
}

class FloorLine(val tiles: List[Tile] = List())