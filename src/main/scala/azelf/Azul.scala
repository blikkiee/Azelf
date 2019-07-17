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

    def placeTilesOnPatternLine(tiles: List[Tile], selectedPatternLine: PatternLine, player: Player) : Player = {
        new Player()
    }

}

class Player{
    val patternLines: List[PatternLine] = List(new PatternLine)
}

class PatternLine{
    def spaces: Int = 1
    def isComplete: Boolean = true
    def tileColour: Tile = Red
}