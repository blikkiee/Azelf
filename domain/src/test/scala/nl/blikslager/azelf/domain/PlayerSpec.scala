package nl.blikslager.azelf.domain

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
        val (player: Player, redundantTiles: List[Tile]) = Player()
                .placeTilesOnPatternLine(List(Red), 1)
                .placeTilesOnPatternLine(List(Green, Green), 2)
                .updateScore
        val wall: Wall = player.wall

        assert(wall.countTiles == 2)
        assert(wall.getTile(1, 3).get == WallTile(Red, true))
        assert(wall.getTile(2, 1).get == WallTile(Green, true))
        assert(wall.getTile(3, 4).get == WallTile(Yellow, false))
        assert(wall.getTile(5, 5).get == WallTile(Blue, false))
        assert(wall.getTile(55, -4) == None)

        assert(player.patternLines(0).filledSpaces == 0)
        assert(player.patternLines(1).filledSpaces == 0)
        assert(redundantTiles == List(Green))
    }
    it should "keep incomplete pattern lines when transfering the tiles from the completed pattern lines to its wall" in {
        val (player: Player, redundantTiles: List[Tile]) = Player()
                .placeTilesOnPatternLine(List(Red), 2)
                .placeTilesOnPatternLine(List(Green, Green), 3)
                .placeTilesOnPatternLine(List(Blue, Blue, Blue, Blue), 4)
                .updateScore
        val wall: Wall = player.wall

        assert(wall.countTiles == 1)
        assert(wall.getTile(2, 4).get == WallTile(Red, false))
        assert(wall.getTile(3, 2).get == WallTile(Green, false))
        assert(wall.getTile(4, 4).get == WallTile(Blue, true))

        assert(player.patternLines(1).filledSpaces == 1)
        assert(player.patternLines(2).filledSpaces == 2)
        assert(player.patternLines(3).filledSpaces == 0)
    }
    it should "redistribute the tiles from its floor line" in {
        val (_, redundantTiles: List[Tile]) = Player()
                .placeTilesOnPatternLine(List(Blue, Blue, Blue, Blue), 1)
                .updateScore
        
        assert(redundantTiles.length == 3)
    }
    it should "not be able to play a tile colour on a pattern line that corresponds to a wall row that already contains that colour, those tiles should end on the floor line" in {
        val (player: Player, _) = Player()
                .placeTilesOnPatternLine(List(Red), 1)
                .updateScore
        val floorLine: FloorLine = player.placeTilesOnPatternLine(List(Red), 1).floorLine
        assert(floorLine.length == 1)
    }

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
    it should "translate to format " +
    "\n\"PatternLine[_ _ _ _ _]\"" +
    "\n\"PatternLine[x _ _][Red]\"" +
    "\nwhen the toString function is called" in {
        val pl_5: PatternLine = new PatternLine(5)
        val (pl_3: PatternLine, _) = new PatternLine(3).fill(List(Red))
        assert("PatternLine[_ _ _ _ _]" == pl_5.toString)
        assert("PatternLine[x _ _][Red]" == pl_3.toString)
    }

    "A wall" should "be able to accept a tile" in {
        val wall: Wall = Wall().placeTile(Red, 1)
        assert(wall.getTile(1,3).get == WallTile(Red, true))
    }
    it should "not matter if a selected row already contains the colour of the new supplied tile" in {
        val wall: Wall = Wall()
            .placeTile(Red, 1)
            .placeTile(Red, 1)
        assert(wall.getTile(1,3).get == WallTile(Red, true))
    }
    it should "ignore non-existent requests for placing tiles" in {
        val wall: Wall = Wall()
            .placeTile(Red, 1)
            .placeTile(Red, 6)
        assert(wall.countTiles == 1)
    }
    it should "be able to calculate its current score (simple)" in {
        val wall: Wall = Wall()
                    .placeTile(Red, 1)
                    .placeTile(Blue, 1)
                    .placeTile(Yellow, 1)
                    .placeTile(Green, 5)
        val score: Int = wall.getScore
        assert(score == 4)
    }
    it should "be able to calculate its current score (hard)" in {
        val wall: Wall = Wall()
                    .placeTile(Red, 1)
                    .placeTile(Blue, 1)
                    .placeTile(Yellow, 1)
                    .placeTile(Purple, 1)
                    .placeTile(Blue, 2)
                    .placeTile(Green, 2)
        val score: Int = wall.getScore
        assert(score == 10)
    }
    it should "be able to calculate its final score (horizontals)" in {
        val wall: Wall = Wall()
                    .placeTile(Red, 1)
                    .placeTile(Blue, 1)
                    .placeTile(Yellow, 1)
                    .placeTile(Green, 1)
                    .placeTile(Purple, 1)
        val score: Int = wall.getScore
        val finalScore: Int = wall.getFinalScore
        assert(score == 5)
        assert(finalScore == 7)
    }
    it should "be able to calculate its final score (vertical)" in {
        val wall: Wall = Wall()
                    .placeTile(Red, 1)
                    .placeTile(Yellow, 2)
                    .placeTile(Blue, 3)
                    .placeTile(Green, 4)
                    .placeTile(Purple, 5)
        val score: Int = wall.getScore
        val finalScore: Int = wall.getFinalScore
        assert(score == 5)
        assert(finalScore == 12)
    }
    it should "be able to calculate its final score (diagonals)" in {
        val wall: Wall = Wall()
                    .placeTile(Red, 1)
                    .placeTile(Red, 2)
                    .placeTile(Red, 3)
                    .placeTile(Red, 4)
                    .placeTile(Red, 5)
        val score: Int = wall.getScore
        val finalScore: Int = wall.getFinalScore
        assert(score == 5)
        assert(finalScore == 15)
    }
    it should "translate to format: " +
    "\n|B _ _ _ _|" +
    "\n|_ B _ _ _|" +
    "\n|_ _ B _ _|" +
    "\n|_ _ _ B _|" +
    "\n|_ _ _ _ B|" +
    "\nwhen the toString function is called" in {
        val emptyWall: Wall = Wall()
        val wall: Wall = Wall()
                    .placeTile(Blue, 1)
                    .placeTile(Yellow, 1)
                    .placeTile(Red, 1)
                    .placeTile(Purple, 1)
                    .placeTile(Green, 1)
                    .placeTile(Blue, 2)
                    .placeTile(Blue, 3)
                    .placeTile(Blue, 4)
                    .placeTile(Blue, 5)
        assert("|_ _ _ _ _|\n|_ _ _ _ _|\n|_ _ _ _ _|\n|_ _ _ _ _|\n|_ _ _ _ _|" == emptyWall.toString)
        assert("|B Y R P G|\n|_ B _ _ _|\n|_ _ B _ _|\n|_ _ _ B _|\n|_ _ _ _ B|" == wall.toString)
    }

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
        val floorLine_3: FloorLine = new FloorLine(List(Purple))

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
    it should "translate to format " +
    "\n\"FloorLine[Red, Green][score: -2]\"" +
    "\nwhen the toString function is called" in {
        val fl_0: FloorLine = new FloorLine()
        val fl_2: FloorLine = new FloorLine(List(Red, Green))
        assert("FloorLine[][score: 0]" == fl_0.toString)
        assert("FloorLine[Red, Green][score: -2]" == fl_2.toString)
    }

    "The score of player" should "be equal to the sum of the wall and the floor line" in {
        val (playerAfterRound1: Player, _) = Player()
            .placeTilesOnPatternLine(List(Red, Red), 1)
            .placeTilesOnPatternLine(List(Green, Green), 2)
            .updateScore
        val wallAfterRound1: Wall = playerAfterRound1.wall

        val (playerAfterRound2: Player, _) = playerAfterRound1
            .placeTilesOnPatternLine(List(Green), 1)
            .updateScore
        val wallAfterRound2: Wall = playerAfterRound2.wall

        assert(wallAfterRound1.countTiles == 2)
        assert(playerAfterRound1.score == 1)
        assert(wallAfterRound2.countTiles == 3)
        assert(playerAfterRound2.score == 4)
    }
}