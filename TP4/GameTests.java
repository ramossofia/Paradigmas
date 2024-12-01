import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

public class GameTests {

    private GameStatus setupGame(List<Player> players, List<Integer> cardValues) {
        List<Card> cards = cardValues.stream()
                .map(value -> new Card(value)) // Assuming initial tokens are 0
                .collect(Collectors.toList());
        return new GameInProgress(players, new Deck(cards));
    }

    @Test
    public void testDeckInitialization() {
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            setupGame(
                    Arrays.asList(
                            new Player("Emilio"),
                            new Player("Julio"),
                            new Player("Bruno")
                    ),
                    Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
            );
        });

        assertEquals("The deck must contain exactly 24 cards.", exception.getMessage());
    }

    @Test
    public void testPlayerCountIsBetweenThreeAndSeven() {
        List<Player> players = Arrays.asList(
                new Player("Emilio"),
                new Player("Julio")
        );

        List<Card> cards = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
                .stream()
                .map(value -> new Card(value)) // Assuming initial tokens are 0
                .collect(Collectors.toList());

        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            new GameInProgress(players, new Deck(cards));
        });

        assertEquals("The number of players should be between 3 and 7.", exception.getMessage());
    }

    @Test
    public void testInitialTokensAndCardsFor3Players() {
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
    public void testInitialTokensAndCardsFor7Players() {
        List<Player> players = Arrays.asList(
                new Player("Emilio"),
                new Player("Julio"),
                new Player("Bruno"),
                new Player("Mariela"),
                new Player("Marcelo"),
                new Player("Kun"),
                new Player("Diego"));
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
    public void testInitialTokensAndCardsFor6Players() {
        List<Player> players = Arrays.asList(
                new Player("Emilio"),
                new Player("Julio"),
                new Player("Bruno"),
                new Player("Mariela"),
                new Player("Marcelo"),
                new Player("Kun")
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
    public void testTurnRotationIsCorrect() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        gameState = gameState.executeAction(new PlaceToken());
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

        gameState = gameState.executeAction(new TakeCard());
        gameState = gameState.executeAction(new PlaceToken());
        gameState = gameState.executeAction(new PlaceToken());
        gameState = gameState.executeAction(new PlaceToken());

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

        gameState = gameState.executeAction(new TakeCard());
        gameState = gameState.executeAction(new PlaceToken());
        gameState = gameState.executeAction(new PlaceToken());

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

        gameState = gameState.executeAction(new TakeCard());
        gameState = gameState.executeAction(new PlaceToken());
        Player nextPlayer = gameState.getPlayers().get(gameState.getCurrentPlayerIndex());

        assertEquals("Julio", nextPlayer.getName(), "The turn should pass to the next player.");
    }

    @Test
    public void testCalculatePointsWithSeries() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        // Simulate Emilio taking the cards 3, 4, and 5
        gameState = gameState.executeAction(new TakeCard()); // Takes 3
        gameState = gameState.executeAction(new TakeCard()); // Takes 4
        gameState = gameState.executeAction(new TakeCard()); // Takes 5

        Player firstPlayer = gameState.getPlayers().get(0);
        int points = firstPlayer.calculateScore();

        assertEquals(-3 + 11, points, "The points should reflect the penalty for the series and the bonus.");
    }

    @Test
    public void testGameEndsWhenDeckIsEmpty() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26) // Initialize with 24 cards
        );
        for (int i = 0; i < 24; i++) {
            gameState = gameState.executeAction(new TakeCard());
        }
        assertTrue(gameState instanceof GameOver, "The game should have ended.");
        assertTrue(gameState.getDeck().isEmpty(), "The deck should be empty.");
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

        // Emilio toma la carta 3 del deck
        gameState = gameState.executeAction(new TakeCard());

        // Emilio pone la carta 3 en la mesa
        gameState = gameState.executeAction(new PlaceCard());

        // Emilio pone un token en la carta 3
        gameState = gameState.executeAction(new PlaceToken());


        // Julio toma la carta 3 con el token
        gameState = gameState.executeAction(new TakeCard());

        Player secondPlayer = gameState.getPlayers().get(1);


        assertEquals(12, secondPlayer.getTokens(), "Julio debería tener 12 tokens.");
    }
    @Test
    public void testPlayerStartsWithoutTokensMustTakeCard() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        // Configuramos a Emilio con 0 tokens
        Player emilio = gameState.getPlayers().get(0);
        emilio.setTokens(0);

        // Ejecutamos la acción de TakeCard
        gameState = gameState.executeAction(new PlaceToken());

        // Verificamos que Emilio ha tomado una carta
        Player updatedPlayer = gameState.getPlayers().get(0);
        assertEquals(1, updatedPlayer.getCards().size(), "El jugador debe haber tomado una carta.");
    }


}
