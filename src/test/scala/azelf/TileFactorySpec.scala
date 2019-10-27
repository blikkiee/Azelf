package Azelf

import org.scalatest.FlatSpec

class TileFactorySpec extends FlatSpec{
    val redTiles: List[Tile]    = List(Red, Red, Red, Red)
    val redTileStock: TileStock = new TileStock(redTiles)

    "The tile factory" should "return red tiles if they are selected from one of its stocks" in {
        val (tileFactory: TileFactory, _) = TileFactory(redTiles, 2)
        val (returnedTiles: List[Tile], _) = tileFactory.selectTiles(Red, redTileStock)
        assert(returnedTiles.distinct == List(Red))
        assert(returnedTiles.length == 4)
    }
    it should "transfer the remaining tiles to its stockpile from one of its stocks" in {
        val tiles: List[Tile]        = List(Red, Red, Green, Blue)
        val (tileFactory: TileFactory, _) = TileFactory(tiles, 2)
        val tileStock: TileStock     = new TileStock(tiles)
        val (returnedTiles: List[Tile], updatedFactory: TileFactory) = tileFactory.selectTiles(Red, tileStock)
        assert(returnedTiles.length == 2)
        assert(updatedFactory.stockpile.length == 2)
        assert(updatedFactory.stockpile.contains(Green))
        assert(updatedFactory.stockpile.contains(Blue))
    }
    it should "not do anything if a tile colour is selected that was not included in the selected TileStock" in {
        val (tileFactory: TileFactory, _) = TileFactory(redTiles, 2)
        val (returnedTiles: List[Tile], updatedFactory: TileFactory) = tileFactory.selectTiles(Blue, redTileStock)
        assert(returnedTiles.isEmpty)
        assert(updatedFactory.stockpile.isEmpty)
        assert(updatedFactory.tileStocks.contains(redTileStock))
    }
    it should "not do anything if a unknown TileStock is presented during tile selection" in {
        val (tileFactory: TileFactory, _) = TileFactory(redTiles, 2)
        val fakeTileStock: TileStock      = new TileStock(List(Blue, Blue, Blue, Blue))
        val (returnedTiles: List[Tile], updatedFactory: TileFactory) = tileFactory.selectTiles(Blue, fakeTileStock)
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
        val (_, updatedFactory: TileFactory) = tileFactory.selectTiles(Red, redTileStock)
        assert(!updatedFactory.tileStocks.contains(redTileStock))
        assert(updatedFactory.tileStocks.length == 2)
        assert(updatedFactory.tileStocks.contains(new TileStock(List(Green, Green, Green, Green))))
    }
    it should "accept only 20 tiles (=> 5 tile stocks) when there are 2 players" in {
        val _50tiles: List[Tile] = Azul.createTileCollection(List(
            (Purple, 10),
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
            (Purple, 10),
            (Blue, 10),
            (Green, 10),
            (Red, 10),
            (Yellow, 10),
        ))
        val (tileFactory: TileFactory, remainingTiles: List[Tile]) = TileFactory(_50tiles, 4)
        assert(tileFactory.tileStocks.map(x => x.tiles).toList.flatten.length == 36)
        assert(remainingTiles.length == 14)
    }
    it should "translate to format " +
    "\n\"TileFactory[" +
    "\n\tTileStock[Red, Red, Red, Red]," + 
    "\n\tTileStock[Green, Green, Green, Green]," + 
    "\n\tStockPile[Green, Purple]" + 
    "\n]\"" +
    "\nwhen the toString function is called" in {
        val tiles: List[Tile] = Azul.createTileCollection(List(
            (Red, 4),
            (Green, 5),
            (Purple, 1),
            (Blue, 2),
        ))
        val (initialTileFactory: TileFactory, _) = TileFactory(tiles, 2)
        val (_, tileFactory: TileFactory) = initialTileFactory.selectTiles(Blue, new TileStock(List(Green, Purple, Blue, Blue)))
        val expectedFirstString: String = "TileFactory[" +
            "\n\tTileStock[Red, Red, Red, Red]," + 
            "\n\tTileStock[Green, Green, Green, Green]," + 
            "\n\tTileStock[Green, Purple, Blue, Blue]," + 
            "\n\tStockPile[]" + 
            "\n]"
        val expectedLastString: String = "TileFactory[" +
            "\n\tTileStock[Red, Red, Red, Red]," + 
            "\n\tTileStock[Green, Green, Green, Green]," + 
            "\n\tStockPile[Green, Purple]" + 
            "\n]"
        assert(expectedFirstString == initialTileFactory.toString)
        assert(expectedLastString == tileFactory.toString)
    }

    "A TileStock" should "(when containing 4 Red tiles) translate to format \"TileStock[Red, Red, Red, Red]\" when the toString function is called" in {
        assert(redTileStock.toString == "TileStock[Red, Red, Red, Red]")
    }
    it should "(when containing 2 Red tiles and 2 Purple tiles) translate to format \"TileStock[Red, Red, Purple, Purple]\" when the toString function is called" in {
        val tileStock = new TileStock(List(Red, Red, Purple, Purple))
        assert(tileStock.toString == "TileStock[Red, Red, Purple, Purple]")
    }
}