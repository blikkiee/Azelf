package Azelf

import org.scalatest.FlatSpec

class PlayerSpec extends FlatSpec{
    "A player" should "put its collected tile on its pattern line" in {
        val tiles: List[Tile] = List(Red)

        val updatedPlayer: Player = Player().placeTilesOnPatternLine(tiles, 1)
        val selectedPatternLine: PatternLine = updatedPlayer.patternLines.find(x => x.spaces == 1).get

        assert(selectedPatternLine.isComplete)
        assert(selectedPatternLine.tileColour == Some(Red))
    }
    it should "put its collected tiles that don't fit on its pattern line in its floor line" in {
        val tiles: List[Tile] = List(Green, Green, Green)

        val updatedPlayer: Player = Player().placeTilesOnPatternLine(tiles, 2)
        val selectedPatternLine: PatternLine = updatedPlayer.patternLines.find(x => x.spaces == 2).get

        assert(selectedPatternLine.isComplete)
        assert(selectedPatternLine.tileColour == Some(Green))
        assert(updatedPlayer.floorLine.tiles.length == 1)
    }
    it should "put its collected tiles that overflow its pattern line in its floor line" in {
        val firstTiles: List[Tile] = List(Green, Green, Green)
        val secondTiles: List[Tile] = firstTiles.tail

        val updatedPlayer: Player = Player().placeTilesOnPatternLine(firstTiles, 4)
        val finalUpdatedPlayer: Player = updatedPlayer.placeTilesOnPatternLine(secondTiles, 4)
        val selectedPatternLine: PatternLine = finalUpdatedPlayer.patternLines.find(x => x.spaces == 4).get

        assert(selectedPatternLine.isComplete)
        assert(selectedPatternLine.tileColour == Some(Green))
        assert(finalUpdatedPlayer.floorLine.tiles.length == 1)
    }
    it should "put its collected tiles that don't match the colour of the selected pattern line on its floorline" in {
        val firstTile: List[Tile] = List(Red)
        val secondTile: List[Tile] = List(Blue, Blue)

        val updatedPlayer: Player = Player().placeTilesOnPatternLine(firstTile, 2)
        val finalUpdatedPlayer: Player = updatedPlayer.placeTilesOnPatternLine(secondTile, 2)
        val selectedPatternLine: PatternLine = finalUpdatedPlayer.patternLines.find(x => x.spaces == 2).get

        assert(!selectedPatternLine.isComplete)
        assert(selectedPatternLine.tileColour == Some(Red))
        assert(selectedPatternLine.filledSpaces == 1)
        assert(finalUpdatedPlayer.floorLine.tiles.length == 2)
    }
    it should "transfer the tiles from its completed pattern lines to its wall" in pending
    it should "redistribute the tiles from its completed pattern lines that are not tranfered to its wall" in pending

    "A wall" should "be able to calculate its current score" in pending

    "A floorline" should "be able to accumulate multiple batches of tiles" in pending
    it should "be able to calculate its current score" in pending
    it should "redistribute its tiles when it is asked for its current score" in pending
}