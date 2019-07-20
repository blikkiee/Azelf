package Azelf

object TileFactory{
    def apply(tiles: List[Tile], nPlayers: Int): (TileFactory, List[Tile]) = {
        val (factoryTiles: List[Tile], remainingTiles: List[Tile]) = tiles.splitAt((1+2*nPlayers)*4)
        (new TileFactory(factoryTiles), remainingTiles)
    }
}

class TileFactory private (tiles: List[Tile], val stockpile: List[Tile] = List()){
    val tileStocks: List[TileStock] = tiles.grouped(4).toList.map(x => new TileStock(x))

    def selectTiles(selectTileColour: Tile, fromTileStock: TileStock): (List[Tile], TileFactory) = {
        def isTileStockInTileFactory(): Boolean = tileStocks.contains(fromTileStock)

        val (returnedTiles: List[Tile], leftOverTiles: List[Tile]) = 
            if(isTileStockInTileFactory) fromTileStock.tiles.partition(x => x == selectTileColour) 
            else (List(), List())
        val leftOverTileStockTiles: List[Tile] = {
            val leftOverTileStocks = tileStocks.filterNot(x => x == fromTileStock)
            (for(stock <- leftOverTileStocks) yield stock.tiles).toList.flatten
        }
        ( returnedTiles, if(returnedTiles.length > 0) new TileFactory(leftOverTileStockTiles, leftOverTiles) else this)
    }

    override def toString(): String = "TileFactory[" +
            tileStocks.mkString("\n\t", ",\n\t", ",") +
            "\n\tStockPile" + stockpile.mkString("[",", ","]") + 
            "\n]"
}

class TileStock(val tiles: List[Tile]){
    override def toString(): String = "TileStock" + tiles.mkString("[",", ","]")
    
    override def equals(other: Any): Boolean = other match {
        case that: TileStock => this.tiles == that.tiles
        case _ => false
    }
}