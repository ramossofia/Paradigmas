import org.junit.jupiter.api.Test;
import java.util.Arrays;
import java.util.List;
import static org.junit.jupiter.api.Assertions.*;

public class GameTests {

    private GameStatus setupGame(List<Player> players, List<Integer> cardValues) {
        Deck deck = new Deck();
        deck.initializeDeck(cardValues);
        return new GameInProgress(players, deck);
    }

    // ** INICIALIZACIÓN **

    @Test
    public void testDeckInitialization() {
        List<Player> players = Arrays.asList(
                new Player("Emilio"),
                new Player("Julio"),
                new Player("Bruno")
        );
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);

        GameStatus gameState = setupGame(players, cardValues);

        assertEquals(24, ((GameInProgress) gameState).getDeck().size(), "El mazo debe contener 24 cartas.");
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
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);

        GameStatus gameState = setupGame(players, cardValues);

        int initialTokens = ((GameInProgress) gameState).calculateInitialTokens(players.size());

        players.forEach(player -> {
            assertEquals(initialTokens, player.getTokens(), "Cada jugador debe recibir " + initialTokens + " fichas al inicio.");
            assertEquals(0, player.getCards().size(), "Cada jugador debe iniciar sin cartas.");
        });
    }

    @Test
    public void testInitialCardRemoval() {
        List<Player> players = Arrays.asList(
                new Player("Emilio"),
                new Player("Julio"),
                new Player("Bruno")
        );
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);

        GameStatus gameState = setupGame(players, cardValues);

        assertEquals(24, ((GameInProgress) gameState).getDeck().size(), "El mazo debe tener 24 cartas tras eliminar 9.");
    }

    // ** TURNOS **

    @Test
    public void testTurnRotationIsCorrect() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);

        GameStatus gameState = setupGame(players, cardValues);

        gameState = gameState.nextPlayer();
        assertEquals(player2, gameState.getPlayers().get(gameState.getCurrentPlayerIndex()), "El turno debería pasar a Julio.");
    }

    @Test
    public void testTurnWrapsAroundToFirstPlayer() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);

        GameStatus gameState = setupGame(players, cardValues);

        gameState = gameState.nextPlayer(); // Turno de Julio
        gameState = gameState.nextPlayer(); // Turno de Bruno
        gameState = gameState.nextPlayer(); // Debe volver a Emilio

        assertEquals(player1, gameState.getPlayers().get(gameState.getCurrentPlayerIndex()), "El turno debe volver al primer jugador.");
    }

    @Test
    public void testPlayerCannotPlayTwiceInARow() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);

        GameStatus gameState = setupGame(players, cardValues);

        gameState = gameState.nextPlayer();
        gameState = gameState.nextPlayer();

        final GameStatus finalGameState = gameState;
        Exception exception = assertThrows(IllegalStateException.class, () -> {
            GameStatus currentGameState = finalGameState;
            if (!currentGameState.getPlayers().get(currentGameState.getCurrentPlayerIndex()).equals(player1)) {
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
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);
        GameStatus gameState = setupGame(players, cardValues);

        gameState = gameState.executeAction(new PlaceToken());

        assertEquals(player2, gameState.getPlayers().get(gameState.getCurrentPlayerIndex()), "El turno debería pasar al siguiente jugador.");
    }

    @Test
    public void testTakeCardWithTokens() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);
        GameStatus gameState = setupGame(players, cardValues);

        // El primer jugador da vuelta la carta y pone un token
        gameState = gameState.executeAction(new PlaceToken());

        // El siguiente jugador toma la carta con el token
        gameState = gameState.nextPlayer();
        gameState = gameState.executeAction(new TakeCard());

        assertEquals(1, player2.getCards().size());
        assertEquals(12, player2.getTokens());
    }

    @Test
    public void testPlayerWithoutTokensMustTakeCard() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);

        GameStatus gameState = setupGame(players, cardValues);

        // El jugador 1 gasta todos sus tokens
        for (int i = 0; i < 11; i++) {
            gameState = gameState.executeAction(new PlaceToken());
            gameState = gameState.nextPlayer();
        }

        // El jugador 1 debe tomar la carta porque no tiene tokens
        gameState = gameState.executeAction(new PlaceToken());
        assertTrue(player1.getCards().contains(3), "El jugador debe tomar la carta porque no tiene tokens.");
    }

    // ** PUNTUACIÓN Y FIN DEL JUEGO **

    @Test
    public void testCalculatePointsWithSeries() {
        Player player = new Player("Emilio");
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);
        player.addCard(6);
        player.addCard(7);
        player.addCard(5);
        int points = player.calculateScore();
        assertEquals(-5, points);
    }

    @Test
    public void testCalculatePointsForSeries() {
        Player player = new Player("Emilio");
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);
        player.addCard(5);
        player.addCard(6);
        player.addCard(7);
        player.addCard(9); // Carta aislada

        int points = player.calculateScore();
        assertEquals(-14, points, "Los puntos deben sumar 5 (serie) + 9 (aislada).");
    }

    @Test
    public void testGameEndsWhenDeckIsEmpty() {
        Player player1 = new Player("Emilio");
        Player player2 = new Player("Julio");
        Player player3 = new Player("Bruno");
        List<Player> players = Arrays.asList(player1, player2, player3);
        List<Integer> cardValues = Arrays.asList(4);

        GameStatus gameState = setupGame(players, cardValues);

        // El primer jugador toma la carta
        gameState = gameState.executeAction(new TakeCard());

        // Verificar que el juego haya terminado
        assertTrue(gameState instanceof GameOver, "El juego debe haber terminado.");
        assertTrue(gameState.getDeck().isEmpty(), "El mazo debe estar vacío.");
    }

    @Test
    public void testInitialTokensForThreeToFivePlayers() {
        List<Player> players = Arrays.asList(
                new Player("Player1"),
                new Player("Player2"),
                new Player("Player3")
        );
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);

        GameStatus game = setupGame(players, cardValues);
        players.forEach(player ->
                assertEquals(11, player.getTokens(), "Con 3 jugadores, cada uno debe tener 11 fichas."));
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
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);

        GameStatus game = setupGame(players, cardValues);
        players.forEach(player ->
                assertEquals(9, player.getTokens(), "Con 6 jugadores, cada uno debe tener 9 fichas."));
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
        List<Integer> cardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);

        GameStatus game = setupGame(players, cardValues);
        players.forEach(player ->
                assertEquals(7, player.getTokens(), "Con 7 jugadores, cada uno debe tener 7 fichas."));
    }
}
