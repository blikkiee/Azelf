package Azelf

import org.scalatest.FlatSpec

class TileFactorySpec extends FlatSpec{
    val redTiles: List[Tile]    = List(Red, Red, Red, Red)
    val redTileStock: TileStock = new TileStock(redTiles)

    "The tile factory" should "return red tiles if they are selected from one of its stocks" in {
        val (tileFactory: TileFactory, _) = TileFactory(redTiles, 2)
        val (returnedTiles: List[Tile], _) = TileFactory.selectTiles(Red, redTileStock, tileFactory)
        assert(returnedTiles.distinct == List(Red))
        assert(returnedTiles.length == 4)
    }
    it should "transfer the remaining tiles to its stockpile from one of its stocks" in {
        val tiles: List[Tile]        = List(Red, Red, Green, Blue)
        val (tileFactory: TileFactory, _) = TileFactory(tiles, 2)
        val tileStock: TileStock     = new TileStock(tiles)
        val (returnedTiles: List[Tile], updatedFactory: TileFactory) = TileFactory.selectTiles(Red, tileStock, tileFactory)
        assert(returnedTiles.length == 2)
        assert(updatedFactory.stockpile.length == 2)
        assert(updatedFactory.stockpile.contains(Green))
        assert(updatedFactory.stockpile.contains(Blue))
    }
    it should "not do anything if a tile colour is selected that was not included in the selected TileStock" in {
        val (tileFactory: TileFactory, _) = TileFactory(redTiles, 2)
        val (returnedTiles: List[Tile], updatedFactory: TileFactory) = TileFactory.selectTiles(Blue, redTileStock, tileFactory)
        assert(returnedTiles.isEmpty)
        assert(updatedFactory.stockpile.isEmpty)
        assert(updatedFactory.tileStocks.contains(redTileStock))
    }
    it should "not do anything if a unknown TileStock is presented during tile selection" in {
        val (tileFactory: TileFactory, _) = TileFactory(redTiles, 2)
        val fakeTileStock: TileStock      = new TileStock(List(Blue, Blue, Blue, Blue))
        val (returnedTiles: List[Tile], updatedFactory: TileFactory) = TileFactory.selectTiles(Blue, fakeTileStock, tileFactory)
        assert(returnedTiles.isEmpty)
        assert(updatedFactory.stockpile.isEmpty)
        assert(updatedFactory.tileStocks.contains(redTileStock))
    }
    it should "when containing 3 TileStocks, still contain 2 full TileStocks after tile selection" in {
        val tiles: List[Tile] = Azul.createTileCollection(List(
            (Red, 4),
            (Blue, 4),
            (Green, 4)
        ))
        val (tileFactory: TileFactory, _) = TileFactory(tiles, 2)
        val (_, updatedFactory: TileFactory) = TileFactory.selectTiles(Red, redTileStock, tileFactory)
        assert(!updatedFactory.tileStocks.contains(redTileStock))
        assert(updatedFactory.tileStocks.length == 2)
        assert(updatedFactory.tileStocks.contains(new TileStock(List(Green, Green, Green, Green))))
    }
    it should "accept only 20 tiles (=> 5 tile stocks) when there are 2 players" in {
        val _50tiles: List[Tile] = Azul.createTileCollection(List(
            (Black, 10),
            (Blue, 10),
            (Green, 10),
            (Red, 10),
            (Yellow, 10),
        ))
        val (tileFactory: TileFactory, remainingTiles: List[Tile]) = TileFactory(_50tiles, 2)
        assert(tileFactory.tileStocks.map(x => x.tiles).toList.flatten.length == 20)
        assert(remainingTiles.length == 30)
    }
    it should "accept only 36 tiles (=> 9 tile stocks) when there are 4 players" in {
        val _50tiles: List[Tile] = Azul.createTileCollection(List(
            (Black, 10),
            (Blue, 10),
            (Green, 10),
            (Red, 10),
            (Yellow, 10),
        ))
        val (tileFactory: TileFactory, remainingTiles: List[Tile]) = TileFactory(_50tiles, 4)
        assert(tileFactory.tileStocks.map(x => x.tiles).toList.flatten.length == 36)
        assert(remainingTiles.length == 14)
    }

    "A TileStock" should "(when containing 4 Red tiles) translate to format \"TileStock[Red, Red, Red, Red]\" when the toString function is called" in {
        assert(redTileStock.toString == "TileStock[Red, Red, Red, Red]")
    }
    it should "(when containing 2 Red tiles and 2 Black tiles) translate to format \"TileStock[Red, Red, Black, Black]\" when the toString function is called" in {
        val tileStock = new TileStock(List(Red, Red, Black, Black))
        assert(tileStock.toString == "TileStock[Red, Red, Black, Black]")
    }
}