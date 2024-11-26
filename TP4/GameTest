
import org.junit.jupiter.api.Test;
import java.util.Arrays;
import java.util.List;
import static org.junit.jupiter.api.Assertions.*;

public class GameTests {

    @Test
    public void testPlayerCannotPlayTwiceInARow() {
        Player player1 = new Player("Alice", 3);
        Player player2 = new Player("Bob", 0);
        List<Player> players = Arrays.asList(player1, player2);
        List<Integer> cards = Arrays.asList(10);

        NoThanks game = new NoThanks();
        game.startGame(players, 2, cards);

        game.executeAction(new PlaceToken(player1));

        try {
            game.executeAction(new PlaceToken(player1));
            fail("Expected IllegalStateException");
        } catch (IllegalStateException e) {
            assertEquals("It's not Alice's turn.", e.getMessage());
        }
    }

    @Test
    public void testCalculatePointsWithSeries() {
        Player player = new Player("TestPlayer", 5);
        player.addCard(new Card(5, 0));
        player.addCard(new Card(6, 0));
        player.addCard(new Card(7, 0));
        int points = player.calculatePoints();
        assertEquals(0, points); 
    }

    @Test
    public void testPlayerCannotPlaceTokenWithoutTokens() {
        Player player = new Player("TestPlayer", 0);
        try {
            player.decrementTokens();
            fail("Expected IllegalStateException");
        } catch (IllegalStateException e) {
            assertEquals("TestPlayer has no tokens left.", e.getMessage());
        }
    }

    @Test
    public void testTakeCardAddsTokensToPlayer() {
        Player player = new Player("TestPlayer", 5);
        Card card = new Card(10, 2); 
        player.takeCard(card, card.getTokens());
        assertEquals(7, player.getTokens()); 
        assertEquals(1, player.getCards().size()); 
    }

    @Test
    public void testPlayerWithoutTokensMustTakeCard() {
        Player player = new Player("Alice", 0); 
        List<Player> players = Arrays.asList(player);

        NoThanks game = new NoThanks();
        game.startGame(players, 0, Arrays.asList(15)); 

        PlayerTurn playerTurn = new PlayerTurn(player, game);
        playerTurn.execute(); 

        assertEquals(-15, player.calculatePoints(), "El jugador debe recibir puntos negativos por la carta.");
        assertEquals(0, game.getDeck().size(), "La carta debe ser retirada del mazo.");
    }

    @Test
    public void testGameEndsWhenDeckIsEmpty() {
        Player player1 = new Player("Alice", 3);
        List<Player> players = Arrays.asList(player1);

        NoThanks game = new NoThanks();
        game.startGame(players, 3, Arrays.asList(10)); 

        game.executeAction(new TakeCard(player1, new Card(10, 0))); 

        assertTrue(game.getDeck().isEmpty(), "El mazo debe estar vacío.");
        game.endGame();
    }

    @Test
    public void testTurnRotationIsCorrect() {
        Player player1 = new Player("Alice", 3);
        Player player2 = new Player("Bob", 3);
        List<Player> players = Arrays.asList(player1, player2);

        NoThanks game = new NoThanks();
        game.startGame(players, 3, Arrays.asList(10, 20));

        game.executeAction(new PlaceToken(player1)); 
        assertEquals(player2, game.getPlayers().get(game.getCurrentPlayerIndex()), "El turno debería pasar a Bob.");
    }


    @Test
    public void testTokensTransferredWithCard() {
        Player player1 = new Player("Alice", 3);
        Player player2 = new Player("Bob", 3);
        List<Player> players = Arrays.asList(player1, player2);

        NoThanks game = new NoThanks();
        game.startGame(players, 3, Arrays.asList(25)); 

        game.executeAction(new PlaceToken(player1)); 
        game.executeAction(new PlaceToken(player2)); 

        game.executeAction(new TakeCard(player1, new Card(25, 2))); 

        assertEquals(5, player1.getTokens(), "Alice debería recibir las fichas de la carta.");
        assertEquals(25, player1.calculatePoints(), "Alice debería tener los puntos de la carta.");
    }

    @Test
    public void testPlayerWithoutTokensIsForcedToTakeCard() {
        Player player1 = new Player("Alice", 0); 
        List<Player> players = Arrays.asList(player1);

        Card card = new Card(10, 0); 
        List<Integer> cards = Arrays.asList(card.getValue());

        NoThanks game = new NoThanks();
        game.startGame(players, 0, cards); 

        game.executeAction(new TakeCard(player1, card));

        assertTrue(player1.getCards().contains(card), "Alice debería haber tomado la carta.");
        assertEquals(10, player1.calculatePoints(), "Alice debería recibir los puntos negativos de la carta.");
    }

    @Test
    public void testSeriesOfCardsCalculatesPointsCorrectly() {
        Player player1 = new Player("Alice", 3);
        List<Player> players = Arrays.asList(player1);

        Card card1 = new Card(15, 0);
        Card card2 = new Card(16, 0);
        Card card3 = new Card(17, 0);
        List<Integer> cards = Arrays.asList(card1.getValue(),card2.getValue());

        NoThanks game = new NoThanks();
        game.startGame(players, 3, cards);

        game.executeAction(new TakeCard(player1, card1));
        game.executeAction(new TakeCard(player1, card2));

        assertEquals(-12, player1.calculatePoints(), "El jugador debe tener 15 puntos negativos por la serie.");
    }


    @Test
    public void testGameEndsWhenAllCardsTaken() {
        Player player1 = new Player("Alice", 3);
        List<Player> players = Arrays.asList(player1);

        Card card1 = new Card(10, 0);
        Card card2 = new Card(20, 0);
        List<Integer> cards = Arrays.asList(card1.getValue(), card2.getValue());

        NoThanks game = new NoThanks();
        game.startGame(players, 3, cards);

        game.executeAction(new TakeCard(player1, card1));
        game.executeAction(new TakeCard(player1, card2));

        game.endGame(); 
        assertTrue(game.getDeck().isEmpty(), "El mazo debe estar vacío al final del juego.");
    }
}
