package Azelf

import org.scalatest.FlatSpec

class AzulSpec extends FlatSpec{
    "The game shuffle functionality" should "shuffle tiles in any order other than the initial order" in {
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
        val fakeTileFactory : TileStock = new TileStock(List(Blue, Blue, Blue, Blue))
        val (returnedTiles: List[Tile], updatedFactory: TileFactory) = Azul.selectTiles(Blue, fakeTileFactory, tileFactory)
        assert(returnedTiles.isEmpty)
        assert(updatedFactory.stockpile.isEmpty)
        assert(updatedFactory.tileStocks.contains(tileStock))
    }
    // todo: multiple TileStocks
}
