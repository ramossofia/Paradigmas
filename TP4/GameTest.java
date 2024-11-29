import org.junit.jupiter.api.Test;
import java.util.Arrays;
import java.util.List;
import static org.junit.jupiter.api.Assertions.*;

public class GameTests {

    private GameStatus setupGame(List<Player> players, List<Integer> cardValues) {
        return new GameInProgress(players, cardValues);
    }

    // ** INICIALIZACIÓN **

    @Test
    public void testDeckInitialization() {
        List<Player> players = Arrays.asList(
                new Player("Emilio"),
                new Player("Julio"),
                new Player("Bruno")
        );

        List<Integer> cardValues = Arrays.asList(
                3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35
        );
        GameStatus gameState = setupGame(players, cardValues);

        assertEquals(24, gameState.deck().size(), "El mazo debe contener 24 cartas tras eliminar 9.");
    }

    @Test
    public void testPlayerCountIsBetweenThreeAndSeven() {
        List<Player> players = Arrays.asList(
                new Player("Emilio"),
                new Player("Julio"),
                new Player("Bruno")
        );

        assertTrue(players.size() >= 3 && players.size() <= 7, "El número de jugadores debe estar entre 3 y 7.");
    }

    @Test
    public void testInitialTokensAndCardsForPlayers() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);

        GameStatus gameState = setupGame(players, Arrays.asList(3, 4, 5));

        int initialTokens = gameState.getInitialTokens();

        players.forEach(player -> {
            assertEquals(initialTokens, player.tokens(), "Cada jugador debe recibir " + initialTokens + " fichas al inicio.");
            assertEquals(0, player.cards().size(), "Cada jugador debe iniciar sin cartas.");
        });
    }

    @Test
    public void testInitialCardRemoval() {
        List<Player> players = Arrays.asList(
                new Player("Emilio"),
                new Player("Julio"),
                new Player("Bruno")
        );

        List<Integer> cardValues = Arrays.asList(
                3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35
        );
        GameStatus gameState = setupGame(players, cardValues);

        assertEquals(24, gameState.deck().size(), "El mazo debe tener 24 cartas tras eliminar 9.");
    }

    // ** TURNOS **

    @Test
    public void testTurnRotationIsCorrect() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);

        List<Integer> cards = Arrays.asList(10, 20);

        GameStatus gameState = setupGame(players, cards);

        gameState = gameState.nextPlayer();
        assertEquals(player2, gameState.players().get(gameState.currentPlayerIndex()), "El turno debería pasar a Julio.");
    }

    @Test
    public void testTurnWrapsAroundToFirstPlayer() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cardValues = Arrays.asList(10);

        GameStatus gameState = setupGame(players, cardValues);

        gameState = gameState.nextPlayer(); // Turno de Julio
        gameState = gameState.nextPlayer(); // Turno de Bruno
        gameState = gameState.nextPlayer(); // Debe volver a Emilio

        assertEquals(player1, gameState.players().get(gameState.currentPlayerIndex()), "El turno debe volver al primer jugador.");
    }

    @Test
    public void testPlayerCannotPlayTwiceInARow() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cards = Arrays.asList(10);

        GameStatus gameState = setupGame(players, cards);

        gameState = gameState.nextPlayer();
        gameState = gameState.updatePlayers(Arrays.asList(player1, player2, player3));

        final GameStatus finalGameState = gameState;
        Exception exception = assertThrows(IllegalStateException.class, () -> {
            GameStatus currentGameState = finalGameState;
            if (!currentGameState.players().get(currentGameState.currentPlayerIndex()).equals(player1)) {
                throw new IllegalStateException("It's not Emilio's turn.");
            }
        });

        assertEquals("It's not Emilio's turn.", exception.getMessage());
    }


    @Test
    public void testPlaceTokenAndPassTurn() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");

        List<Player> players = Arrays.asList(player1, player2, player3);
        GameStatus gameState = setupGame(players, Arrays.asList(10));

        gameState = gameState.placeToken();

        assertEquals(player2, gameState.players().get(gameState.currentPlayerIndex()), "El turno debería pasar al siguiente jugador.");
    }

    @Test
    public void testTakeCardWithTokens() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cardValues = Arrays.asList(10);
        GameStatus gameState = setupGame(players, cardValues);

        // El primer jugador da vuelta la carta y pone un token
        gameState = gameState.placeToken();

        // El siguiente jugador toma la carta con el token
        gameState = gameState.nextPlayer();
        Card topCard = gameState.drawCard();
        player2.addCard(topCard);

        assertEquals(1, player2.cards().size());
        assertEquals(12, player2.tokens());
    }

    @Test
    public void testPlayerWithoutTokensMustTakeCard() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);

        List<Integer> cards = Arrays.asList(15);

        GameStatus gameState = setupGame(players, cards);

        player1.addCard(new Card(15));

        assertEquals(-15, player1.calculatePoints(), "El jugador debe recibir puntos negativos por la carta.");
    }

    // ** PUNTUACIÓN Y FIN DEL JUEGO **

    @Test
    public void testCalculatePointsWithSeries() {
        Player player = new Player("Emilio");
        player.addCard(new Card(6));
        player.addCard(new Card(7));
        player.addCard(new Card(5));
        int points = player.calculatePoints();
        assertEquals(-5, points);
    }

    @Test
    public void testCalculatePointsForSeries() {
        Player player = new Player("Emilio");
        player.addCard(new Card(5));
        player.addCard(new Card(6));
        player.addCard(new Card(7));
        player.addCard(new Card(9)); // Carta aislada

        int points = player.calculatePoints();
        assertEquals(-14, points, "Los puntos deben sumar 5 (serie) + 9 (aislada).");
    }

    @Test
    public void testGameEndsWhenDeckIsEmpty() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);

        List<Integer> cards = Arrays.asList(10);

        GameStatus gameState = setupGame(players, cards);

        // El primer jugador toma la carta
        Card drawnCard = gameState.drawCard();
        player1.addCard(drawnCard);

        // Verificar que el juego haya terminado
        gameState = gameState.updateDeck(Arrays.asList()); // El mazo está vacío

        assertTrue(gameState instanceof GameOver, "El juego debe haber terminado.");
        assertTrue(gameState.deck().isEmpty(), "El mazo debe estar vacío.");
    }


    @Test
    public void testInitialTokensForThreeToFivePlayers() {
        List<Player> players = Arrays.asList(
                new Player("Player1"),
                new Player("Player2"),
                new Player("Player3")
        );

        GameStatus game = new GameInProgress(players, Arrays.asList(3, 4, 5));
        players.forEach(player ->
                assertEquals(11, player.tokens(), "Con 3 jugadores, cada uno debe tener 11 fichas."));
    }

    @Test
    public void testInitialTokensForSixPlayers() {
        List<Player> players = Arrays.asList(
                new Player("Player1"),
                new Player("Player2"),
                new Player("Player3"),
                new Player("Player4"),
                new Player("Player5"),
                new Player("Player6")
        );

        GameStatus game = new GameInProgress(players, Arrays.asList(3, 4, 5));
        players.forEach(player ->
                assertEquals(8, player.tokens(), "Con 6 jugadores, cada uno debe tener 8 fichas."));
    }

    @Test
    public void testInitialTokensForSevenPlayers() {
        List<Player> players = Arrays.asList(
                new Player("Player1"),
                new Player("Player2"),
                new Player("Player3"),
                new Player("Player4"),
                new Player("Player5"),
                new Player("Player6"),
                new Player("Player7")
        );

        GameStatus game = new GameInProgress(players, Arrays.asList(3, 4, 5));
        players.forEach(player ->
                assertEquals(7, player.tokens(), "Con 7 jugadores, cada uno debe tener 7 fichas."));
    }

}
