import org.junit.jupiter.api.Test;
import java.util.Arrays;
import java.util.List;
import static org.junit.jupiter.api.Assertions.*;

public class GameTests {

    private GameStatus setupGame(List<Player> players, List<Integer> cardValues, int initialTokens) {
        return new GameStatus(players, cardValues, 0);
    }

    // ** INICIALIZACIÓN **

    @Test
    public void testDeckInitialization() {
        List<Player> players = Arrays.asList(
                new Player("Emilio", 0),
                new Player("Julio", 0),
                new Player("Bruno", 0)
        );

        List<Integer> cardValues = Arrays.asList(
                3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35
        );
        GameStatus gameState = setupGame(players, cardValues, 11);

        assertEquals(24, gameState.deck().size(), "El mazo debe contener 24 cartas tras eliminar 9.");
    }

    @Test
    public void testPlayerCountIsBetweenThreeAndSeven() {
        List<Player> players = Arrays.asList(
                new Player("Emilio", 0),
                new Player("Julio", 0),
                new Player("Bruno", 0)
        );

        assertTrue(players.size() >= 3 && players.size() <= 7, "El número de jugadores debe estar entre 3 y 7.");
    }

    @Test
    public void testInitialTokensAndCardsForPlayers() {
        Player player1 = new Player("Emilio", 0);
        Player player2 = new Player("Julio", 0);
        Player player3 = new Player("Bruno", 0);
        List<Player> players = Arrays.asList(player1, player2, player3);

        int initialTokens = 11;

        players.forEach(player -> {
            assertEquals(initialTokens, player.tokens(), "Cada jugador debe recibir 11 fichas al inicio.");
            assertEquals(0, player.cards().size(), "Cada jugador debe iniciar sin cartas.");
        });
    }

    @Test
    public void testInitialCardRemoval() {
        List<Player> players = Arrays.asList(
                new Player("Emilio", 0),
                new Player("Julio", 0),
                new Player("Bruno", 0)
        );

        List<Integer> cardValues = Arrays.asList(
                3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35
        );
        GameStatus gameState = setupGame(players, cardValues, 11);

        assertEquals(24, gameState.deck().size(), "El mazo debe tener 24 cartas tras eliminar 9.");
    }

    // ** TURNOS **

    @Test
    public void testTurnRotationIsCorrect() {
        Player player1 = new Player("Emilio", 3);
        Player player2 = new Player("Julio", 3);
        Player player3 = new Player("Bruno", 3);
        List<Player> players = Arrays.asList(player1, player2, player3);

        List<Integer> cards = Arrays.asList(10, 20);

        GameStatus gameState = setupGame(players, cards, 3);

        gameState = gameState.nextPlayer();
        assertEquals(player2, gameState.players().get(gameState.currentPlayerIndex()), "El turno debería pasar a Julio.");
    }

    @Test
    public void testTurnWrapsAroundToFirstPlayer() {
        Player player1 = new Player("Emilio", 3);
        Player player2 = new Player("Julio", 3);
        Player player3 = new Player("Bruno", 3);
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cardValues = Arrays.asList(10);

        GameStatus gameState = setupGame(players, cardValues, 3);

        gameState = gameState.nextPlayer(); // Turno de Julio
        gameState = gameState.nextPlayer(); // Turno de Bruno
        gameState = gameState.nextPlayer(); // Debe volver a Emilio

        assertEquals(player1, gameState.players().get(gameState.currentPlayerIndex()), "El turno debe volver al primer jugador.");
    }

    @Test
    public void testPlayerCannotPlayTwiceInARow() {
        Player player1 = new Player("Emilio", 3);
        Player player2 = new Player("Julio", 0);
        Player player3 = new Player("Bruno", 0);
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cards = Arrays.asList(10);

        GameStatus gameState = setupGame(players, cards, 2);

        gameState = gameState.nextPlayer();
        gameState = gameState.updatePlayers(Arrays.asList(player1, player2, player3));

        final GameStatus finalGameState = gameState;
        Exception exception = assertThrows(IllegalStateException.class, () -> {
            if (!finalGameState.players().get(finalGameState.currentPlayerIndex()).equals(player1)) {
                throw new IllegalStateException("It's not Emilio's turn.");
            }
        });

        assertEquals("It's not Emilio's turn.", exception.getMessage());
    }

    @Test
    public void testPlayerCannotPlaceMultipleTokens() {
        Player player = new Player("Emilio", 3);
        List<Player> players = List.of(player);
        List<Integer> cardValues = List.of(10);
        GameStatus gameState = setupGame(players, cardValues, 3);

        // Primera ficha colocada correctamente
        gameState = gameState.placeToken();

        // Intentar colocar una segunda ficha en el mismo turno
        Exception exception = assertThrows(IllegalStateException.class, () -> {
            gameState = gameState.placeToken();
        });

        assertEquals("Cannot place more than one token per turn.", exception.getMessage(), "El mensaje de error debe indicar que no se pueden colocar múltiples fichas.");
        assertEquals(2, player.tokens(), "El jugador debe seguir teniendo 2 fichas después del intento fallido.");
    }

    // ** COLOCACIÓN DE FICHAS Y CARTAS **

    @Test
    public void testPlaceTokenAndPassTurn() {
        Player player1 = new Player("Emilio", 3);
        Player player2 = new Player("Julio", 3);
        List<Player> players = Arrays.asList(player1, player2);
        GameStatus gameState = setupGame(players, Arrays.asList(10), 3);

        gameState = gameState.placeToken();

        assertEquals(player2, gameState.players().get(gameState.currentPlayerIndex()), "El turno debería pasar al siguiente jugador.");
    }

    @Test
    public void testTakeCardWithTokens() {
        Player player = new Player("Emilio", 3);
        List<Player> players = Arrays.asList(player);
        List<Integer> cardValues = Arrays.asList(10);
        GameStatus gameState = setupGame(players, cardValues, 3);

        gameState = gameState.addTokenToCard(2); // Agrega 2 fichas
        Card topCard = gameState.drawCard();

        player = player.addCard(topCard);
        assertEquals(5, player.tokens(), "El jugador debería ganar las 2 fichas de la carta.");
        assertEquals(1, player.cards().size(), "El jugador debería tener la carta en su colección.");
    }

    @Test
    public void testPlayerWithoutTokensMustTakeCard() {
        Player player1 = new Player("Emilio", 0);
        Player player2 = new Player("Julio", 3);
        Player player3 = new Player("Bruno", 3);
        List<Player> players = Arrays.asList(player1, player2, player3);

        List<Integer> cards = Arrays.asList(15);

        GameStatus gameState = setupGame(players, cards, 0);

        player1 = player1.addCard(new Card(15, 0));

        assertEquals(-15, player1.calculatePoints(), "El jugador debe recibir puntos negativos por la carta.");
    }

    // ** PUNTUACIÓN Y FIN DEL JUEGO **

    @Test
    public void testCalculatePointsWithSeries() {
        Player player = new Player("Emilio", 5);
        player = player.addCard(new Card(5, 0));
        player = player.addCard(new Card(6, 0));
        player = player.addCard(new Card(7, 0));
        int points = player.calculatePoints();
        assertEquals(0, points);
    }

    @Test
    public void testCalculatePointsForSeries() {
        Player player = new Player("Emilio", 0);
        player = player.addCard(new Card(5, 0));
        player = player.addCard(new Card(6, 0));
        player = player.addCard(new Card(7, 0));
        player = player.addCard(new Card(9, 0)); // Carta aislada

        int points = player.calculatePoints();
        assertEquals(14, points, "Los puntos deben sumar 5 (serie) + 9 (aislada).");
    }

    @Test
    public void testGameEndsWhenDeckIsEmpty() {
        Player player1 = new Player("Emilio", 3);
        Player player2 = new Player("Julio", 3);
        Player player3 = new Player("Bruno", 3);
        List<Player> players = Arrays.asList(player1, player2, player3);

        List<Integer> cards = Arrays.asList(10);

        GameStatus gameState = setupGame(players, cards, 3);

        gameState = gameState.updateDeck(Arrays.asList()); // El mazo está vacío

        assertTrue(gameState.deck().isEmpty(), "El mazo debe estar vacío.");
    }
}
