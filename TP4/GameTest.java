import org.junit.jupiter.api.Test;
import java.util.Arrays;
import java.util.List;
import static org.junit.jupiter.api.Assertions.*;

public class GameTests {

    @Test
    public void testGameSetup() {
        Player player1 = new Player("Emilio", 0);
        Player player2 = new Player("Julio", 0);
        Player player3 = new Player("Bruno", 0);
        List<Player> players = Arrays.asList(player1, player2, player3);

        List<Integer> cardValues = Arrays.asList(
                3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26
        );

        NoThanks game = new NoThanks();
        game.startGame(players, 11, cardValues);

        assertEquals(24, game.getDeck().size(), "El mazo debe contener 24 cartas.");
        assertTrue(players.size() >= 3, "Debe haber al menos 3 jugadores.");
        int expectedTokens = 11;
        for (Player player : players) {
            assertEquals(expectedTokens, player.getTokens(), "Cada jugador debe tener " + expectedTokens + " fichas.");
        }
    }

    @Test
    public void testGameSetupInvalidNumberOfPlayers() {
        Player player1 = new Player("Emilio", 0);
        List<Player> players = Arrays.asList(player1);

        List<Integer> cardValues = Arrays.asList(
                3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26
        );

        NoThanks game = new NoThanks();
        Exception exception = assertThrows(IllegalArgumentException.class, () -> {
            game.startGame(players, 11, cardValues);
        });

        assertEquals("The number of players must be between 3 and 7.", exception.getMessage());
    }

    @Test
    public void testGameSetupInvalidCardValues() {
        Player player1 = new Player("Emilio", 0);
        Player player2 = new Player("Julio", 0);
        Player player3 = new Player("Bruno", 0);
        List<Player> players = Arrays.asList(player1, player2, player3);

        List<Integer> cardValues = Arrays.asList(
                2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 36
        );

        NoThanks game = new NoThanks();
        Exception exception = assertThrows(IllegalArgumentException.class, () -> {
            game.startGame(players, 11, cardValues);
        });

        assertEquals("Card values must be between 3 and 35.", exception.getMessage());
    }

    @Test
    public void testPlayerCannotPlayTwiceInARow() {
        Player player1 = new Player("Emilio", 3);
        Player player2 = new Player("Julio", 0);
        Player player3 = new Player("Bruno", 0);
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cards = Arrays.asList(10);

        NoThanks game = new NoThanks();
        game.startGame(players, 2, cards);

        game.executeAction(new PlaceToken(player1));

        try {
            game.executeAction(new PlaceToken(player1));
            fail("Expected IllegalStateException");
        } catch (IllegalStateException e) {
            assertEquals("It's not Emilio's turn.", e.getMessage());
        }
    }

    @Test
    public void testCalculatePointsWithSeries() {
        Player player = new Player("Emilio", 5);
        player.addCard(new Card(5, 0));
        player.addCard(new Card(6, 0));
        player.addCard(new Card(7, 0));
        int points = player.calculatePoints();
        assertEquals(0, points);
    }

    @Test
    public void testPlayerCannotPlaceTokenWithoutTokens() {
        Player player = new Player("Emilio", 0);
        try {
            player.decrementTokens();
            fail("Expected IllegalStateException");
        } catch (IllegalStateException e) {
            assertEquals("Emilio has no tokens left.", e.getMessage());
        }
    }

    @Test
    public void testTakeCardAddsTokensToPlayer() {
        Player player = new Player("Emilio", 5);
        Card card = new Card(10, 2);
        player.takeCard(card, card.getTokens());
        assertEquals(7, player.getTokens());
        assertEquals(1, player.getCards().size());
    }

    @Test
    public void testPlayerWithoutTokensMustTakeCard() {
        Player player1 = new Player("Emilio", 0);
        Player player2 = new Player("Julio", 3);
        Player player3 = new Player("Bruno", 3);
        List<Player> players = Arrays.asList(player1, player2, player3);

        NoThanks game = new NoThanks();
        game.startGame(players, 0, Arrays.asList(15));

        PlayerTurn playerTurn = new PlayerTurn(player1, game);
        playerTurn.execute();

        assertEquals(-15, player1.calculatePoints(), "El jugador debe recibir puntos negativos por la carta.");
        assertEquals(0, game.getDeck().size(), "La carta debe ser retirada del mazo.");
    }

    @Test
    public void testGameEndsWhenDeckIsEmpty() {
        Player player1 = new Player("Emilio", 3);
        Player player2 = new Player("Julio", 3);
        Player player3 = new Player("Bruno", 3);
        List<Player> players = Arrays.asList(player1, player2, player3);

        NoThanks game = new NoThanks();
        game.startGame(players, 3, Arrays.asList(10));

        game.executeAction(new TakeCard(player1, new Card(10, 0)));

        assertTrue(game.getDeck().isEmpty(), "El mazo debe estar vacío.");
        game.endGame();
    }

    @Test
    public void testTurnRotationIsCorrect() {
        Player player1 = new Player("Emilio", 3);
        Player player2 = new Player("Julio", 3);
        Player player3 = new Player("Bruno", 3);
        List<Player> players = Arrays.asList(player1, player2, player3);

        NoThanks game = new NoThanks();
        game.startGame(players, 3, Arrays.asList(10, 20));

        game.executeAction(new PlaceToken(player1));
        assertEquals(player2, game.getPlayers().get(game.getCurrentPlayerIndex()), "El turno debería pasar a Julio.");
    }



    @Test
    public void testGameEndsWhenAllCardsTaken() {
        Player player1 = new Player("Emilio", 3);
        Player player2 = new Player("Julio", 3);
        Player player3 = new Player("Bruno", 3);
        List<Player> players = Arrays.asList(player1, player2, player3);

        Card card1 = new Card(10, 0);
        Card card2 = new Card(20, 0);
        List<Integer> cards = Arrays.asList(card1.getValue(), card2.getValue());

        NoThanks game = new NoThanks();
        game.startGame(players, 3, cards);

        game.executeAction(new TakeCard(player1, card1));
        game.advanceToNextPlayer();
        game.executeAction(new TakeCard(player2, card2));

        game.endGame();
        assertTrue(game.getDeck().isEmpty(), "El mazo debe estar vacío al final del juego.");
    }

    @Test
    public void testPlayerWithoutTokensIsForcedToTakeCard() {
        Player player1 = new Player("Emilio", 0);
        Player player2 = new Player("Julio", 3);
        Player player3 = new Player("Bruno", 3);
        List<Player> players = Arrays.asList(player1, player2, player3);

        Card card = new Card(10, 0);
        List<Integer> cards = Arrays.asList(card.getValue());

        NoThanks game = new NoThanks();
        game.startGame(players, 0, cards);

        game.executeAction(new TakeCard(player1, card));

        assertTrue(player1.getCards().contains(card), "Emilio should have taken the card.");
        assertEquals(-10, player1.calculatePoints(), "Emilio should receive the negative points from the card.");
    }

    @Test
    public void testSeriesOfCardsCalculatesPointsCorrectly() {
        Player player1 = new Player("Emilio", 3);
        Player player2 = new Player("Julio", 3);
        Player player3 = new Player("Bruno", 3);
        List<Player> players = Arrays.asList(player1, player2, player3);

        Card card1 = new Card(15, 0);
        Card card2 = new Card(16, 0);
        Card card3 = new Card(17, 0);
        List<Integer> cards = Arrays.asList(card1.getValue(), card2.getValue(), card3.getValue());

        NoThanks game = new NoThanks();
        game.startGame(players, 3, cards);

        game.executeAction(new TakeCard(player1, card1));
        game.advanceToNextPlayer();
        game.advanceToNextPlayer(); // Skip player2's turn
        game.advanceToNextPlayer(); // Skip player3's turn
        game.executeAction(new TakeCard(player1, card2));
        game.advanceToNextPlayer();
        game.advanceToNextPlayer(); // Skip player2's turn
        game.advanceToNextPlayer(); // Skip player3's turn
        game.executeAction(new TakeCard(player1, card3));

        assertEquals(-15, player1.calculatePoints(), "El jugador debe tener 15 puntos negativos por la serie.");
    }

    @Test
    public void testTokensTransferredWithCard() {
        Player player1 = new Player("Emilio", 3);
        Player player2 = new Player("Julio", 3);
        Player player3 = new Player("Bruno", 3);
        List<Player> players = Arrays.asList(player1, player2, player3);

        Card card = new Card(25, 2);
        List<Integer> cards = Arrays.asList(card.getValue());

        NoThanks game = new NoThanks();
        game.startGame(players, 3, cards);

        game.executeAction(new PlaceToken(player1));
        game.advanceToNextPlayer();
        game.executeAction(new PlaceToken(player2));
        game.advanceToNextPlayer();
        game.advanceToNextPlayer(); // Skip player3's turn
        game.executeAction(new TakeCard(player1, card));

        assertEquals(5, player1.getTokens(), "Emilio debería recibir las fichas de la carta.");
        assertEquals(-25, player1.calculatePoints(), "Emilio debería tener los puntos de la carta.");
    }
}
