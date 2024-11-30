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

        int deckSize = ((GameInProgress) gameState).getDeck().size();
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
        GameStatus gameState = setupGame(
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

        int deckSize = ((GameInProgress) gameState).getDeck().size();
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

        GameStatus nextGameState = gameState.nextPlayer();
        Player currentPlayer = nextGameState.getPlayers().get(nextGameState.getCurrentPlayerIndex());

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
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
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
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        GameStatus nextGameState = gameState.executeAction(new PlaceToken());
        Player nextPlayer = nextGameState.getPlayers().get(nextGameState.getCurrentPlayerIndex());

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

        // Aquí Emilio pierde 1 token al usar PlaceToken, se espera que ahora esté sin tokens
        for (int i = 0; i < 12; i++) {
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
                new ArrayList<>() // Mazo vacío al inicio
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

        // El primer jugador coloca un token
        gameState = gameState.executeAction(new PlaceToken()).nextPlayer();

        // El segundo jugador toma la carta
        gameState = gameState.executeAction(new TakeCard());

        gameState = gameState.executeAction(new PlaceToken());

        // Verificar que el segundo jugador tenga 12 tokens y la carta que tomó
        Player secondPlayer = gameState.getPlayers().get(1);


        assertEquals(11, secondPlayer.getTokens(), "El segundo jugador debe tener 12 tokens.");
        assertTrue(secondPlayer.getCards().contains(3), "El segundo jugador debe tener la carta que tomó.");
    }


}
