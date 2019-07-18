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

        val updatedPlayer: Player = Player().placeTilesOnPatternLine(firstTiles, 2)
        val finalUpdatedPlayer: Player = updatedPlayer.placeTilesOnPatternLine(secondTiles, 2)
        val selectedPatternLine: PatternLine = finalUpdatedPlayer.patternLines.find(x => x.spaces == 2).get

        assert(selectedPatternLine.isComplete)
        assert(selectedPatternLine.tileColour == Some(Green))
        assert(finalUpdatedPlayer.floorLine.tiles.length == 3)
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
    it should "transfer the tiles from its completed pattern lines to its wall" in {
        val player: Player = Player()
                .placeTilesOnPatternLine(List(Red), 1)
                .placeTilesOnPatternLine(List(Green, Green), 2)
                .coverWall
        val wall: Wall = player.wall

        assert(wall.countTiles == 2)
        assert(wall.tile(1, 3).get == (Red, true))
        assert(wall.tile(2, 1).get == (Green, true))
        assert(wall.tile(3, 4).get == (Yellow, false))
        assert(wall.tile(5, 5).get == (Blue, false))
        assert(wall.tile(55, -4) == None)

        // assert patternLine 1 and 2 are empty
        // extra test to verify that pattern lines keep incomplete pattern lines
    }
    it should "redistribute the tiles from its completed pattern lines that are not tranfered to its wall" in pending

    "A pattern line" should "provide the tiles it contains if the it completed and asked to cover the wall" in {
        val patternLine: PatternLine = new PatternLine(1)
        val (filledPatternLine: PatternLine, _) = patternLine.fill(List(Red))
        val (emptyPatternLine: PatternLine, tiles: List[Tile]) = filledPatternLine.empty

        assert(tiles.length == filledPatternLine.filledSpaces)
        assert(tiles.head == filledPatternLine.tileColour.get)
        assert(emptyPatternLine.filledSpaces == 0)
        assert(emptyPatternLine.tileColour == None)
    }
    it should "not provide the tiles it contains if the it incompleted and asked to cover the wall" in {
        val patternLine: PatternLine = new PatternLine(2)
        val (filledPatternLine: PatternLine, _) = patternLine.fill(List(Red))
        val (nonEmptyPatternLine: PatternLine, tiles: List[Tile]) = filledPatternLine.empty

        assert(tiles.length == 0)
        assert(nonEmptyPatternLine.filledSpaces == filledPatternLine.filledSpaces)
        assert(nonEmptyPatternLine.tileColour == filledPatternLine.tileColour)
    }

    "A wall" should "be able to accept a tile" in {
        val wall: Wall = Wall().placeTile(Red, 1)
        assert(wall.tile(1,3).get == (Red, true))
    }
    it should "not matter if a selected row already contains the colour of the new supplied tile" in pending
    it should "be able to calculate its current score" in pending

    "A floor line" should "be able to accumulate multiple batches of tiles" in {
        val firstFloorLine: FloorLine = new FloorLine
        val secondFloorLine: FloorLine = firstFloorLine.fill(List(Red))
        val finalFloorLine: FloorLine = secondFloorLine.fill(List(Green, Green))

        assert(firstFloorLine.length == 0)
        assert(secondFloorLine.length == 1)
        assert(finalFloorLine.length == 3)
    }
    it should "be equal to another floor line if both contain the same tiles" in {
        val floorLine_1: FloorLine = new FloorLine(List(Red))
        val floorLine_2: FloorLine = new FloorLine(List(Red))
        val floorLine_3: FloorLine = new FloorLine(List(Black))

        assert(floorLine_1 == floorLine_2)
        assert(floorLine_1 != floorLine_3)
    }
    it should "be able to calculate its current score" in {
        val floorLine_1: FloorLine = new FloorLine(List(Red))
        val floorLine_2: FloorLine = floorLine_1.fill(List(Red))
        val floorLine_3: FloorLine = floorLine_2.fill(List(Red))
        val floorLine_5: FloorLine = floorLine_3.fill(List(Red, Red))
        val floorLine_6: FloorLine = floorLine_5.fill(List(Red))
        val floorLine_10: FloorLine = floorLine_6.fill(List(Red, Red, Red, Red))

        assert(floorLine_1.peekScore == -1)
        assert(floorLine_2.peekScore == -2)
        assert(floorLine_3.peekScore == -4)
        assert(floorLine_5.peekScore == -8)
        assert(floorLine_6.peekScore == -11)
        assert(floorLine_10.peekScore == -23)
    }
    it should "redistribute its tiles when it is asked for its current score" in {
        val tiles_1: List[Tile] = List(Blue)
        val tiles_2: List[Tile] = List(Red, Green)
        val floorLine: FloorLine = new FloorLine(tiles_1).fill(tiles_2)

        val (score: Int, tiles: List[Tile], resetFloorLine: FloorLine) = floorLine.score
        val resetScore: Int = resetFloorLine.peekScore

        assert(score == -4)
        assert(tiles == tiles_1 :++ tiles_2)
        assert(resetFloorLine == new FloorLine)
        assert(resetScore == 0)
    }
}