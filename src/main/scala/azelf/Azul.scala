package Azelf

import scala.util._

// Create collection with tiles (10x black, 10x blue, 10x green, 10x red, 10x yellow) and shuffle it
// Distribute tiles to tilefactory

object Azul {
    def shuffleTiles(tiles : List[Tile]) : List[Tile] = {
        def shuffleRecursive() : List[Tile] = {
            val shuffled = Random.shuffle(tiles)
            if (shuffled == tiles) shuffleRecursive else shuffled
        }
        if (tiles.distinct.length > 1) shuffleRecursive() else tiles
    }

    def selectTiles(selectTileColour : Tile, fromTileStock : TileStock, fromTileFactory : TileFactory) : (List[Tile], TileFactory) = {
        (fromTileStock.tiles.filter(x => x == selectTileColour), fromTileFactory)
        // partition
    }
}

class TileFactory(tiles : List[Tile])
// ? wil je eigenlijk wel exceptions gooien?
// (depending on amount of players) TileFactory should receive a maximum of ((1+2*nPlayer)*4) 36 tiles
// it should recieve at least 1 tile

class TileStock(val tiles : List[Tile])
// ? wil je eigenlijk wel exceptions gooien?
// it should recieve at least 1 tile