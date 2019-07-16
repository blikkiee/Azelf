package Azelf

import scala.util._

// Create collection with tiles (10x black, 10x blue, 10x green, 10x red, 10x yellow) and shuffle it
// Distribute tiles to tilefactory

// todo: refactor; leesbaarder maken van functies: wat (voor stappen) doe je?

object Azul {
    def createTileCollection(specification : List[(Tile, Int)]) : List[Tile] = {
        (for ( (colour : Tile, amount : Int) <- specification) yield (for (i <- 1 to amount) yield colour).toList).flatten
    }

    def shuffleTiles(tiles : List[Tile]) : List[Tile] = {
        def shuffleRecursive() : List[Tile] = {
            val shuffled = Random.shuffle(tiles)
            if (shuffled == tiles) shuffleRecursive else shuffled
        }
        if (tiles.distinct.length > 1) shuffleRecursive() else tiles
    }

    def selectTiles(selectTileColour : Tile, fromTileStock : TileStock, fromTileFactory : TileFactory) : (List[Tile], TileFactory) = {
        def isTileStockInTileFactory() : Boolean = fromTileFactory.tileStocks.contains(fromTileStock)

        val (returnedTiles : List[Tile], leftOverTiles : List[Tile]) = 
            if(isTileStockInTileFactory) fromTileStock.tiles.partition(x => x == selectTileColour) 
            else (List(), List())
        ( returnedTiles, if(returnedTiles.length > 0) new TileFactory(List(), leftOverTiles) else fromTileFactory)
    }
}

class TileFactory(tiles : List[Tile], val stockpile : List[Tile] = List()){
    val tileStocks : List[TileStock] = List(new TileStock(tiles))
}

class TileStock(val tiles : List[Tile]){
    override def toString() : String = "TileStock" + tiles.mkString("[",", ","]")
    
    override def equals(other: Any): Boolean = other match {
        case that: TileStock => this.tiles == that.tiles
        case _ => false
    }
}