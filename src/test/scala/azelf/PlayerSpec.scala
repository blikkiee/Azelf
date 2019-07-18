package Azelf

import org.scalatest.FlatSpec

class PlayerSpec extends FlatSpec{
    "A player" should "put its collected tile on its pattern line" in {
        val tiles: List[Tile] = List(Red)

        val updatedPlayer: Player = Player.placeTilesOnPatternLine(tiles, 1, Player())
        val selectedPatternLine: PatternLine = updatedPlayer.patternLines.find(x => x.spaces == 1).get

        assert(selectedPatternLine.isComplete)
        assert(selectedPatternLine.tileColour == Some(Red))
    }
    it should "put its collected tiles that don't fit on its pattern line in its floor line" in {
        val tiles: List[Tile] = List(Green, Green, Green)

        val updatedPlayer: Player = Player.placeTilesOnPatternLine(tiles, 2, Player())
        val selectedPatternLine: PatternLine = updatedPlayer.patternLines.find(x => x.spaces == 2).get

        assert(selectedPatternLine.isComplete)
        assert(selectedPatternLine.tileColour == Some(Green))
        assert(updatedPlayer.floorLine.tiles.length == 1)
    }
    it should "put its collected tiles that don't match the colour of the selected pattern line on its floorline" in {
        pending
    }
    it should "transfer the tiles from its completed pattern lines to its wall" in pending
    it should "redistribute the tiles from its completed pattern lines that are not tranfered to its wall" in pending

    "A wall" should "be able to calculate its current score" in pending

    "A floorline" should "be able to calculate its current score" in pending
    it should "redistribute its tiles when it is asked for its current score" in pending
}