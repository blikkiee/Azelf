package Azelf

import org.scalatest.FlatSpec

class AzulSpec extends FlatSpec{
    "The createTileCollection function" should "accept a list of tuples (colour, amount) and produce a list of tiles" in {
        val tileRequirements : List[(Tile, Int)] = List( 
            (Red, 5),
            (Blue, 5),
            (Green, 5)
        )
        val tiles : List[Tile] = Azul.createTileCollection(tileRequirements)
        assert(tiles.length == 15)
        assert(tiles.filter(x => x == Green).length == 5)
        assert(tiles.filter(x => x == Red).length == 5)
    }
    
    "The shuffle functionality of the game" should "shuffle tiles in any order other than the initial order" in {
        val initialOrder : List[Tile] = List( Red, Blue, Black, Green, Yellow, Yellow, Green, Black, Blue, Red )
        val shuffled : List[Tile]     = Azul.shuffleTiles(initialOrder)
        assert(shuffled != initialOrder)
        assert(shuffled.length == initialOrder.length)
    }
    // this test is to make sure that the unittest for shuffling might never fail due to chance
    it should "always present an other order of tiles, even with small collections of tiles" in { 
        val initialOrder : List[Tile]  = List( Red, Blue )
        val shuffled : List[Tile]      = Azul.shuffleTiles(initialOrder)
        val shuffledAgain : List[Tile] = Azul.shuffleTiles(shuffled)
        assert(shuffled != initialOrder)
        assert(shuffledAgain == initialOrder)
    }
    it should "not try to shuffle tiles if there are only tiles of one colour" in {
        val initialOrder : List[Tile] = List( Red, Red )
        val shuffled : List[Tile]     = Azul.shuffleTiles(initialOrder)
    }
    it should "be able to handle empty collections of tiles" in {
        val empty : List[Tile] = List()
        assert(empty == Azul.shuffleTiles(empty))
    }

    "The tile factory" should "return red tiles if they are selected from one of its stocks" in {
        val tiles : List[Tile]        = List(Red, Red, Red, Red)
        val tileFactory : TileFactory = new TileFactory(tiles)
        val tileStock : TileStock     = new TileStock(tiles)
        val (returnedTiles: List[Tile], _) = Azul.selectTiles(Red, tileStock, tileFactory)
        assert(returnedTiles.distinct == List(Red))
        assert(returnedTiles.length == 4)
    }
    it should "transfer the remaining tiles to its stockpile from one of its stocks" in {
        val tiles : List[Tile]        = List(Red, Red, Green, Blue)
        val tileFactory : TileFactory = new TileFactory(tiles)
        val tileStock : TileStock     = new TileStock(tiles)
        val (returnedTiles: List[Tile], updatedFactory: TileFactory) = Azul.selectTiles(Red, tileStock, tileFactory)
        assert(returnedTiles.length == 2)
        assert(updatedFactory.stockpile.length == 2)
        assert(updatedFactory.stockpile.contains(Green))
        assert(updatedFactory.stockpile.contains(Blue))
    }
    it should "not do anything if a tile colour is selected that was not included in the selected TileStock" in {
        val tiles : List[Tile]        = List(Red, Red, Red, Red)
        val tileFactory : TileFactory = new TileFactory(tiles)
        val tileStock : TileStock     = new TileStock(tiles)
        val (returnedTiles: List[Tile], updatedFactory: TileFactory) = Azul.selectTiles(Blue, tileStock, tileFactory)
        assert(returnedTiles.isEmpty)
        assert(updatedFactory.stockpile.isEmpty)
        assert(updatedFactory.tileStocks.contains(tileStock))
    }
    it should "not do anything if a unknown TileStock is presented during tile selection" in {
        val tiles : List[Tile]          = List(Red, Red, Red, Red)
        val tileFactory : TileFactory   = new TileFactory(tiles)
        val tileStock : TileStock       = new TileStock(tiles)
        val fakeTileStock : TileStock   = new TileStock(List(Blue, Blue, Blue, Blue))
        val (returnedTiles: List[Tile], updatedFactory: TileFactory) = Azul.selectTiles(Blue, fakeTileStock, tileFactory)
        assert(returnedTiles.isEmpty)
        assert(updatedFactory.stockpile.isEmpty)
        assert(updatedFactory.tileStocks.contains(tileStock))
    }
    it should "when containing multiple TileStocks still contain full TileStocks after tile selection" in {
        val tiles : List[Tile] = Azul.createTileCollection(List(
            (Red, 4),
            (Blue, 4),
            (Green, 4)
        ))
        val tileFactory : TileFactory = new TileFactory(tiles)
        val selectedTileStock : TileStock = new TileStock(List(Red, Red, Red, Red))
        val (_, updatedFactory : TileFactory) = Azul.selectTiles(Red, selectedTileStock, tileFactory)
        assert(updatedFactory.tileStocks.length == 2)
        assert(updatedFactory.tileStocks.contains(new TileStock(List(Green, Green, Green, Green))))
    }
    // todo: multiple TileStocks

    "A TileStock" should "(when containing 4 Red tiles) translate to format \"TileStock[Red, Red, Red, Red]\" when the toString function is called" in {
        val tileStock = new TileStock(List(Red, Red, Red, Red))
        assert(tileStock.toString == "TileStock[Red, Red, Red, Red]")
    }
    it should "(when containing 2 Red tiles and 2 Black tiles) translate to format \"TileStock[Red, Red, Black, Black]\" when the toString function is called" in {
        val tileStock = new TileStock(List(Red, Red, Black, Black))
        assert(tileStock.toString == "TileStock[Red, Red, Black, Black]")
    }
}
