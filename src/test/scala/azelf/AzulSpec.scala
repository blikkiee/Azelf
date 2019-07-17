package Azelf

import org.scalatest.FlatSpec

class AzulSpec extends FlatSpec{
    "The createTileCollection function" should "accept a list of tuples (colour, amount) and produce a list of tiles" in {
        val tileRequirements: List[(Tile, Int)] = List( 
            (Red, 5),
            (Blue, 5),
            (Green, 5)
        )
        val tiles: List[Tile] = Azul.createTileCollection(tileRequirements)
        assert(tiles.length == 15)
        assert(tiles.filter(x => x == Green).length == 5)
        assert(tiles.filter(x => x == Red).length == 5)
    }
    
    "The shuffle functionality of the game" should "shuffle tiles in any order other than the initial order" in {
        val initialOrder: List[Tile] = List( Red, Blue, Black, Green, Yellow, Yellow, Green, Black, Blue, Red )
        val shuffled: List[Tile]     = Azul.shuffleTiles(initialOrder)
        assert(shuffled != initialOrder)
        assert(shuffled.length == initialOrder.length)
    }
    // this test is to make sure that the unittest for shuffling might never fail due to chance
    it should "always present an other order of tiles, even with small collections of tiles" in { 
        val initialOrder: List[Tile]  = List( Red, Blue )
        val shuffled: List[Tile]      = Azul.shuffleTiles(initialOrder)
        val shuffledAgain: List[Tile] = Azul.shuffleTiles(shuffled)
        assert(shuffled != initialOrder)
        assert(shuffledAgain == initialOrder)
    }
    it should "not try to shuffle tiles if there are only tiles of one colour" in {
        val initialOrder: List[Tile] = List( Red, Red )
        val shuffled: List[Tile]     = Azul.shuffleTiles(initialOrder)
    }
    it should "be able to handle empty collections of tiles" in {
        val empty: List[Tile] = List()
        assert(empty == Azul.shuffleTiles(empty))
    }
}
