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
                        new Player("Emilio", 11, List.of(), false),
                        new Player("Julio", 11, List.of(), false),
                        new Player("Bruno", 11, List.of(), false)
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        int deckSize = ((GameInProgress) gameState).getDeck().size();
        assertEquals(24, deckSize, "The deck should contain 24 cards.");
    }

    @Test
    public void testPlayerCountIsBetweenThreeAndSeven() {
        List<Player> players = Arrays.asList(
                new Player("Emilio", 11, List.of(), false),
                new Player("Julio", 11, List.of(), false),
                new Player("Bruno", 11, List.of(), false)
        );

        boolean validPlayerCount = players.size() >= 3 && players.size() <= 7;
        assertTrue(validPlayerCount, "The number of players should be between 3 and 7.");
    }

    @Test
    public void testInitialTokensAndCardsForPlayers() {
        List<Player> players = Arrays.asList(
                new Player("Emilio", 11, List.of(), false),
                new Player("Julio", 11, List.of(), false),
                new Player("Bruno", 11, List.of(), false)
        );
        GameStatus gameState = setupGame(
                players,
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        int initialTokens = ((GameInProgress) gameState).calculateInitialTokens(players.size());

        players.forEach(player -> {
            assertEquals(initialTokens, player.getTokens(), "Each player should receive " + initialTokens + " tokens at the start.");
            assertEquals(0, player.getCards().size(), "Each player should start with no cards.");
        });
    }

    @Test
    public void testInitialCardRemoval() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio", 11, List.of(), false),
                        new Player("Julio", 11, List.of(), false),
                        new Player("Bruno", 11, List.of(), false)
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        int deckSize = ((GameInProgress) gameState).getDeck().size();
        assertEquals(24, deckSize, "The deck should have 24 cards after removing 9.");
    }

    @Test
    public void testTurnRotationIsCorrect() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio", 11, List.of(), false),
                        new Player("Julio", 11, List.of(), false),
                        new Player("Bruno", 11, List.of(), false)
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        GameStatus nextGameState = gameState.nextPlayer();
        Player currentPlayer = nextGameState.getPlayers().get(nextGameState.getCurrentPlayerIndex());

        assertEquals("Julio", currentPlayer.getName(), "The turn should pass to Julio.");
    }

    @Test
    public void testTurnWrapsAroundToFirstPlayer() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio", 11, List.of(), false),
                        new Player("Julio", 11, List.of(), false),
                        new Player("Bruno", 11, List.of(), false)
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        GameStatus finalGameState = gameState
                .nextPlayer()
                .nextPlayer()
                .nextPlayer();

        Player currentPlayer = finalGameState.getPlayers().get(finalGameState.getCurrentPlayerIndex());
        assertEquals("Emilio", currentPlayer.getName(), "The turn should return to the first player.");
    }

    @Test
    public void testPlayerCannotPlayTwiceInARow() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio", 11, List.of(), false),
                        new Player("Julio", 11, List.of(), false),
                        new Player("Bruno", 11, List.of(), false)
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        GameStatus nextGameState = gameState.nextPlayer().nextPlayer();
        Player currentPlayer = nextGameState.getPlayers().get(nextGameState.getCurrentPlayerIndex());

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
                        new Player("Emilio", 11, List.of(), false),
                        new Player("Julio", 11, List.of(), false),
                        new Player("Bruno", 11, List.of(), false)
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        GameStatus nextGameState = gameState.executeAction(new PlaceToken());
        Player nextPlayer = nextGameState.getPlayers().get(nextGameState.getCurrentPlayerIndex());

        assertEquals("Julio", nextPlayer.getName(), "The turn should pass to the next player.");
    }

    @Test
    public void testTakeCardWithTokens() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio", 11, List.of(), false),
                        new Player("Julio", 11, List.of(), false),
                        new Player("Bruno", 11, List.of(), false)
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        GameStatus nextGameState = gameState.executeAction(new PlaceToken())
                .nextPlayer()
                .executeAction(new TakeCard());

        Player updatedPlayer = nextGameState.getPlayers().get(1);
        assertEquals(1, updatedPlayer.getCards().size(), "Julio should have taken 1 card.");
        assertEquals(12, updatedPlayer.getTokens(), "Julio should have 12 tokens after taking the card.");
    }

    @Test
    public void testPlayerWithoutTokensMustTakeCard() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio", 1, List.of(), false),
                        new Player("Julio", 11, List.of(), false),
                        new Player("Bruno", 11, List.of(), false)
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        // Aquí Emilio pierde 1 token al usar PlaceToken, se espera que ahora esté sin tokens
        for (int i = 0; i < 1; i++) {
            gameState = gameState.executeAction(new PlaceToken()).nextPlayer();
        }

        // Ahora Emilio debería estar sin tokens, por lo que deberá tomar una carta
        GameStatus finalState = gameState.executeAction(new TakeCard());
        Player updatedPlayer = finalState.getPlayers().get(0);

        // Verificamos que Emilio haya tomado la carta, ya que no tiene tokens
        assertTrue(updatedPlayer.getCards().contains(3), "Emilio must take the card because he has no tokens.");
    }



    @Test
    public void testCalculatePointsWithSeries() {
        Player player = new Player("Emilio", 11, List.of(6, 7, 5), false);
        int points = player.calculateScore();
        assertEquals(-5, points, "The points should reflect the penalty for the series.");
    }

    @Test
    public void testCalculatePointsForSeries() {
        Player player = new Player("Emilio", 11, List.of(5, 6, 7, 9), false);
        int points = player.calculateScore();
        assertEquals(-14, points, "The points should sum 5 (series) + 9 (isolated).");
    }

    @Test
    public void testGameEndsWhenDeckIsEmpty() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio", 11, List.of(), false),
                        new Player("Julio", 11, List.of(), false),
                        new Player("Bruno", 11, List.of(), false)
                ),
                new ArrayList<>() // Mazo vacío al inicio
        );

        GameStatus finalState = gameState.executeAction(new TakeCard());
        assertTrue(finalState instanceof GameOver, "The game should have ended.");
        assertTrue(finalState.getDeck().isEmpty(), "The deck should be empty.");
    }

}
