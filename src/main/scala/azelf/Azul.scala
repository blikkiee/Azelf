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

    def placeTilesOnPatternLine(tiles: List[Tile], selectedPatternLine: Int, player: Player) : Player = {
        val patternLine: PatternLine = new PatternLine(selectedPatternLine, Option(tiles.head))
        val updatedPatternLines: List[PatternLine] = player.patternLines.updated(selectedPatternLine-1, patternLine)
        val remainingTiles: List[Tile] = tiles.drop(selectedPatternLine)
        new Player(new FloorLine(remainingTiles), updatedPatternLines)
    }
}

class Player private(
    val floorLine: FloorLine, 
    val patternLines: List[PatternLine])

class PatternLine(val spaces: Int, val tileColour: Option[Tile] = None){
    def isComplete: Boolean = true
}

class FloorLine(val tiles: List[Tile] = List())