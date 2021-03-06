package nl.blikslager.azelf.domain

import scala.annotation.tailrec
import scala.util._
import scala.io.StdIn._

// Create collection with tiles (10x purple, 10x blue, 10x green, 10x red, 10x yellow) and shuffle it
// Distribute tiles to tilefactory

// todo: refactor; leesbaarder maken van functies: wat (voor stappen) doe je?

object Azul {
    // ? dictionary ipv list van tuples?
    def createTileCollection(specification: List[(Tile, Int)]): List[Tile] = {
        (for ( (colour: Tile, amount: Int) <- specification) yield (for (i <- 1 to amount) yield colour).toList).flatten
    }

    def shuffleTiles(tiles: List[Tile]): List[Tile] = {
        @tailrec def shuffleRecursive(): List[Tile] = {
            val shuffled = Random.shuffle(tiles)
            if (shuffled == tiles) shuffleRecursive else shuffled
        }
        if (tiles.distinct.length > 1) shuffleRecursive() else tiles
    }
}