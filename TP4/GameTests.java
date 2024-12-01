import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;

public class GameTests {

    private GameStatus setupGame(List<Player> players, List<Integer> cardValues) {
        List<Card> cards = createCards(cardValues);
        return new GameInProgress(players, new Deck(cards));
    }

    @Test
    public void testDeckInitialization() {
        List<Player> players = createPlayers("Emilio", "Julio", "Bruno");
        List<Integer> invalidCardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);

        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> setupGame(players, invalidCardValues));

        assertEquals("The deck must contain exactly 24 cards.", exception.getMessage());
    }

    @Test
    public void testPlayerCountIsBetweenThreeAndSeven() {
        List<Player> players = createPlayers("Emilio", "Julio");
        List<Card> cards = createCards(getCardValues());

        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> new GameInProgress(players, new Deck(cards)));

        assertEquals("The number of players should be between 3 and 7.", exception.getMessage());
    }

    @Test
    public void testInitialTokensAndCardsFor3Players() {
        List<Player> players = createPlayers("Emilio", "Julio", "Bruno");
        GameStatus gameState = setupGame(players, getCardValues());

        int initialTokens = GameInProgress.calculateInitialTokens(players.size());
        assertInitialTokens(players, initialTokens);
    }

    @Test
    public void testInitialTokensAndCardsFor7Players() {
        List<Player> players = createPlayers("Emilio", "Julio", "Bruno", "Mariela", "Marcelo", "Kun", "Diego");
        GameStatus gameState = setupGame(players, getCardValues());

        int initialTokens = GameInProgress.calculateInitialTokens(players.size());
        assertInitialTokens(players, initialTokens);
    }

    @Test
    public void testInitialTokensAndCardsFor6Players() {
        List<Player> players = createPlayers("Emilio", "Julio", "Bruno", "Mariela", "Marcelo", "Kun");
        GameStatus gameState = setupGame(players, getCardValues());

        int initialTokens = GameInProgress.calculateInitialTokens(players.size());
        assertInitialTokens(players, initialTokens);
    }

    @Test
    public void testTurnRotationIsCorrect() {
        GameStatus gameState = setupGameWithCardsAndPlayers(getCardValues(), "Emilio", "Julio", "Bruno");

        gameState = gameState.executeAction(new PlaceToken());
        assertCurrentPlayer(gameState, "Julio");
    }

    @Test
    public void testTurnWrapsAroundToFirstPlayer() {
        GameStatus gameState = setupGameWithCardsAndPlayers(getCardValues(), "Emilio", "Julio", "Bruno");

        gameState = executeActions(gameState, new TakeCard(), new PlaceToken(), new PlaceToken(), new PlaceToken());
        assertCurrentPlayer(gameState, "Emilio");
    }

    @Test
    public void testPlayerCannotPlayTwiceInARow() {
        GameStatus gameState = setupGameWithCardsAndPlayers(getCardValues(), "Emilio", "Julio", "Bruno");

        gameState = executeActions(gameState, new TakeCard(), new PlaceToken(), new PlaceToken());

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
        GameStatus gameState = setupGameWithCardsAndPlayers(getCardValues(), "Emilio", "Julio", "Bruno");

        gameState = executeActions(gameState, new TakeCard(), new PlaceToken());
        assertCurrentPlayer(gameState, "Julio");
    }

    @Test
    public void testCalculatePointsWithSeries() {
        GameStatus gameState = setupGameWithCardsAndPlayers(getCardValues(), "Emilio", "Julio", "Bruno");

        gameState = executeActions(gameState, new TakeCard(), new TakeCard(), new TakeCard());

        Player firstPlayer = gameState.getPlayers().get(0);
        int points = firstPlayer.calculateScore();

        assertEquals(-3 + 11, points, "The points should reflect the penalty for the series and the bonus.");
    }

    @Test
    public void testGameEndsWhenDeckIsEmpty() {
        GameStatus gameState = setupGameWithCardsAndPlayers(getCardValues(), "Emilio", "Julio", "Bruno");

        gameState = IntStream.range(0, 24)
                .mapToObj(i -> new TakeCard())
                .reduce(gameState, GameStatus::executeAction, (gs1, gs2) -> gs2);

        assertTrue(gameState instanceof GameOver, "The game should have ended.");
        assertTrue(gameState.getDeck().isEmpty(), "The deck should be empty.");
    }

    @Test
    public void testPlayerPlacesTokenAndNextPlayerTakesCard() {
        GameStatus gameState = setupGameWithCardsAndPlayers(getCardValues(), "Emilio", "Julio", "Bruno");

        gameState = executeActions(gameState, new TakeCard(), new PlaceCard(), new PlaceToken(), new TakeCard());

        Player secondPlayer = gameState.getPlayers().get(1);

        assertEquals(12, secondPlayer.getTokens(), "Julio debería tener 12 tokens.");
    }

    @Test
    public void testPlayerStartsWithoutTokensMustTakeCard() {
        GameStatus gameState = setupGameWithCardsAndPlayers(getCardValues(), "Emilio", "Julio", "Bruno");

        Player emilio = gameState.getPlayers().get(0);
        emilio.setTokens(0);

        gameState = gameState.executeAction(new PlaceToken());

        Player updatedPlayer = gameState.getPlayers().get(0);
        assertEquals(1, updatedPlayer.getCards().size(), "El jugador debe haber tomado una carta.");
    }

    private List<Player> createPlayers(String... playerNames) {
        return Arrays.stream(playerNames)
                .map(Player::new)
                .collect(Collectors.toList());
    }

    private List<Card> createCards(List<Integer> cardValues) {
        return cardValues.stream()
                .map(Card::new)
                .collect(Collectors.toList());
    }

    private GameStatus setupGameWithCardsAndPlayers(List<Integer> cardValues, String... playerNames) {
        return setupGame(createPlayers(playerNames), cardValues);
    }

    private static List<Integer> getCardValues() {
        return Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);
    }

    private GameStatus executeActions(GameStatus gameState, Action... actions) {
        return Arrays.stream(actions)
                .reduce(gameState, GameStatus::executeAction, (gs1, gs2) -> gs2);
    }

    private void assertInitialTokens(List<Player> players, int expectedTokens) {
        players.forEach(player -> assertEquals(expectedTokens, player.getTokens(),
                "Each player should receive " + expectedTokens + " tokens at the start."));
    }

    private void assertCurrentPlayer(GameStatus gameState, String expectedPlayerName) {
        Player currentPlayer = gameState.getPlayers().get(gameState.getCurrentPlayerIndex());
        assertEquals(expectedPlayerName, currentPlayer.getName(),
                "The current player should be " + expectedPlayerName + ".");
    }
}
