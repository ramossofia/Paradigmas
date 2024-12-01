
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
                new Player("Julio")
        );

        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            new GameInProgress(players, new Deck(Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)));
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
    public void testInitialCardDeck() {
        GameStatus gameState = setupGame(
                Arrays.asList(
                        new Player("Emilio"),
                        new Player("Julio"),
                        new Player("Bruno")
                ),
                Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)
        );

        int deckSize = (gameState).getDeck().size();
        assertEquals(24, deckSize);
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

    //ARREGLAR
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

        gameState = gameState.executeAction(new TakeCard());
        for (int i = 0; i < 12; i++) {
            gameState = gameState.executeAction(new PlaceToken());
            gameState.nextPlayer();
        }

        gameState = gameState.executeAction(new PlaceToken());
        Player updatedPlayer = gameState.getPlayers().get(0);

        assertTrue(updatedPlayer.getCards().contains(3), "Emilio must take the card because he has no tokens.");
    }

//TAKECARD ESTA FUNCIONANDO MAL
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
        System.out.println("Player before adding tokens: " + gameState.getPlayers().get(0).getCards());
        gameState = gameState.executeAction(new TakeCard()); // Takes 4
        System.out.println("Player before adding tokens: " + gameState.getPlayers().get(0).getCards());
        gameState = gameState.executeAction(new TakeCard()); // Takes 5
        System.out.println("Player before adding tokens: " + gameState.getPlayers().get(0).getCards());

        Player firstPlayer = gameState.getPlayers().get(0);
        int points = firstPlayer.calculateScore();

        assertEquals(-3 + 11, points, "The points should reflect the penalty for the series and the bonus.");
    }

//ESTE TEST ES RARO, POR QUE PUEDO HACER TAKECARD SI NO HAY CARTAS
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


        gameState = gameState.executeAction(new TakeCard());
        gameState = gameState.executeAction(new PlaceToken());
        gameState = gameState.executeAction(new TakeCard());

        Player secondPlayer = gameState.getPlayers().get(1);

        assertTrue(secondPlayer.getCards().contains(3));
        assertEquals(12, secondPlayer.getTokens(), "The second player should have 12 tokens.");
        ;

    }
}
