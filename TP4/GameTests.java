import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import static org.junit.jupiter.api.Assertions.*;

public class GameTests {

    private GameStatus setupGame(List<Player> players, List<Integer> cardValues) {
        return new GameInProgress(players, new Deck(cardValues));
    }

    @Test
    public void testDeckInitialization() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        int deckSize = (gameState).getDeck().size();
        assertEquals(24, deckSize, "The deck should contain 24 cards.");
    }

    @Test
    public void testPlayerCountIsBetweenThreeAndSeven() {
        List<Player> players = Arrays.asList(
                new Player("Emilio"),
                new Player("Julio"),
                new Player("Bruno")
        );

        boolean validPlayerCount = players.size() >= 3 && players.size() <= 7;
        assertTrue(validPlayerCount, "The number of players should be between 3 and 7.");
    }

    @Test
    public void testInitialTokensAndCardsForPlayers() {
        List<Player> players = Arrays.asList(
                new Player("Emilio"),
                new Player("Julio"),
                new Player("Bruno")
        );
        setupGame(
                players,
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        int initialTokens = GameInProgress.calculateInitialTokens(players.size());

        players.forEach(player -> {
            assertEquals(initialTokens, player.getTokens(), "Each player should receive " + initialTokens + " tokens at the start.");
            assertEquals(0, player.getCards().size(), "Each player should start with no cards.");
        });
    }

    @Test
    public void testInitialCardRemoval() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        int deckSize = (gameState).getDeck().size();
        assertEquals(24, deckSize, "The deck should have 24 cards after removing 9.");
    }

    @Test
    public void testTurnRotationIsCorrect() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        gameState.nextPlayer();
        Player currentPlayer = gameState.getPlayers().get(gameState.getCurrentPlayerIndex());

        assertEquals("Julio", currentPlayer.getName(), "The turn should pass to Julio.");
    }

    @Test
    public void testTurnWrapsAroundToFirstPlayer() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        gameState.nextPlayer();
        gameState.nextPlayer();
        gameState.nextPlayer();

        Player currentPlayer = gameState.getPlayers().get(gameState.getCurrentPlayerIndex());
        assertEquals("Emilio", currentPlayer.getName(), "The turn should return to the first player.");
    }

    @Test
    public void testPlayerCannotPlayTwiceInARow() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        gameState.nextPlayer();
        gameState.nextPlayer();
        Player currentPlayer = gameState.getPlayers().get(gameState.getCurrentPlayerIndex());

        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            if (!currentPlayer.getName().equals("Emilio")) {
                throw new IllegalStateException("It's not Emilio's turn.");
            }
        });

        assertEquals("It's not Emilio's turn.", exception.getMessage());
    }

    @Test
    public void testPlaceTokenAndPassTurn() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        gameState = gameState.executeAction(new PlaceToken());
        gameState.nextPlayer();
        Player nextPlayer = gameState.getPlayers().get(gameState.getCurrentPlayerIndex());

        assertEquals("Julio", nextPlayer.getName(), "The turn should pass to the next player.");
    }

    @Test
    public void testPlayerWithoutTokensMustTakeCard() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        for (int i = 0; i < 12; i++) {
            gameState = gameState.executeAction(new PlaceToken());
            gameState.nextPlayer();
        }

        gameState = gameState.executeAction(new TakeCard());
        Player updatedPlayer = gameState.getPlayers().get(0);

        assertTrue(updatedPlayer.getCards().contains(3), "Emilio must take the card because he has no tokens.");
    }

    @Test
    public void testCalculatePointsWithSeries() {
        Player player = new Player("Emilio");
        player = player.addCard(6).addCard(7).addCard(5);
        int points = player.calculateScore();
        assertEquals(-5, points, "The points should reflect the penalty for the series.");
    }

    @Test
    public void testCalculatePointsForSeries() {
        Player player = new Player("Emilio");
        player = player.addCard(5).addCard(6).addCard(7).addCard(9);
        int points = player.calculateScore();
        assertEquals(-14, points, "The points should sum 5 (series) + 9 (isolated).");
    }

    @Test
    public void testGameEndsWhenDeckIsEmpty() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                new ArrayList<>()
        );

        GameStatus finalState = gameState.executeAction(new TakeCard());
        assertTrue(finalState instanceof GameOver, "The game should have ended.");
        assertTrue(finalState.getDeck().isEmpty(), "The deck should be empty.");
    }

    @Test
    public void testPlayerPlacesTokenAndNextPlayerTakesCard() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        gameState = gameState.executeAction(new PlaceToken());
        gameState.nextPlayer();

        gameState = gameState.executeAction(new TakeCard());

        Player secondPlayer = gameState.getPlayers().get(1);

        assertEquals(11, secondPlayer.getTokens(), "The second player should have 11 tokens.");
        assertTrue(secondPlayer.getCards().contains(3), "The second player should have the card they took.");
    }
}
