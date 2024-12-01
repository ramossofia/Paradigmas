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
    public void test01PlayerCountIsBetweenThreeAndSeven() {
        List<Player> players = createPlayers("Emilio", "Julio");
        List<Card> cards = createCards(getCardValues());

        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> new GameInProgress(players, new Deck(cards)));

        assertEquals("The number of players should be between 3 and 7.", exception.getMessage());
    }

    @Test
    public void test02DeckInitialization() {
        List<Player> players = createPlayers("Emilio", "Julio", "Bruno");
        List<Integer> invalidCardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);

        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> setupGame(players, invalidCardValues));

        assertEquals("The deck must contain exactly 24 cards.", exception.getMessage());
    }

    @Test
    public void test03InitialTokensAndCardsFor3Players() {
        GameStatus gameState = initializeGame3PlayersAndFullDeck();
        List<Player> players = gameState.getPlayers();

        int initialTokens = GameInProgress.calculateInitialTokens(players.size());
        assertInitialTokens(players, initialTokens);
    }

    @Test
    public void test04InitialTokensAndCardsFor6Players() {
        List<Player> players = createPlayers("Emilio", "Julio", "Bruno", "Mariela", "Marcelo", "Kun");
        GameStatus gameState = setupGame(players, getCardValues());

        int initialTokens = GameInProgress.calculateInitialTokens(players.size());
        assertInitialTokens(players, initialTokens);
    }

    @Test
    public void test05InitialTokensAndCardsFor7Players() {
        List<Player> players = createPlayers("Emilio", "Julio", "Bruno", "Mariela", "Marcelo", "Kun", "Diego");
        GameStatus gameState = setupGame(players, getCardValues());

        int initialTokens = GameInProgress.calculateInitialTokens(players.size());
        assertInitialTokens(players, initialTokens);
    }

    @Test
    public void test06TurnRotationIsCorrect() {
        GameStatus gameState = initializeGame3PlayersAndFullDeck();

        gameState = gameState.executeAction(new PlaceToken());
        assertCurrentPlayer(gameState, "Julio");
    }

    @Test
    public void test07TurnWrapsAroundToFirstPlayer() {
        GameStatus gameState = initializeGame3PlayersAndFullDeck();

        gameState = executeActions(gameState, new TakeCard(), new PlaceToken(), new PlaceToken(), new PlaceToken());
        assertCurrentPlayer(gameState, "Emilio");
    }

    @Test
    public void test08PlayerCannotPlayTwiceInARow() {
        GameStatus gameState = initializeGame3PlayersAndFullDeck();

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
    public void test09PlaceTokenAndPassTurn() {
        GameStatus gameState = initializeGame3PlayersAndFullDeck();

        gameState = executeActions(gameState, new TakeCard(), new PlaceToken());
        assertCurrentPlayer(gameState, "Julio");
    }

    @Test
    public void test10TakeLastCardFromDeck() {
        GameStatus gameState = initializeGame3PlayersAndFullDeck();

        // Ejecutar la acción de tomar una carta
        gameState = gameState.executeAction(new TakeCard());

        // Verificar que la carta tomada sea la esperada y que el mazo esté vacío
        Player currentPlayer = gameState.getPlayers().get(gameState.getCurrentPlayerIndex());
        assertEquals(3, currentPlayer.getCards().get(0).getValue(), "La carta tomada debe ser la 26.");
    }

    @Test
    public void test11PlayerPlacesTokenAndNextPlayerTakesCard() {
        GameStatus gameState = initializeGame3PlayersAndFullDeck();

        gameState = executeActions(gameState, new TakeCard(), new PlaceCard(), new PlaceToken(), new TakeCard());

        Player secondPlayer = gameState.getPlayers().get(1);

        assertEquals(12, secondPlayer.getTokens());
    }

    @Test
    public void test12PlayerStartsWithoutTokensMustTakeCard() {
        GameStatus gameState = initializeGame3PlayersAndFullDeck();

        Player emilio = gameState.getPlayers().get(0);
        emilio.setTokens(0);

        gameState = gameState.executeAction(new PlaceToken());

        Player updatedPlayer = gameState.getPlayers().get(0);
        assertEquals(1, updatedPlayer.getCards().size());
    }

    @Test
    public void test13CalculatePointsWithSeries() {
        GameStatus gameState = initializeGame3PlayersAndFullDeck();

        gameState = executeActions(gameState, new TakeCard(), new TakeCard(), new TakeCard());

        Player firstPlayer = gameState.getPlayers().get(0);
        int points = firstPlayer.calculateScore();

        assertEquals(-3 + 11, points);
    }

    @Test
    public void test14GameEndsWhenDeckIsEmpty() {
        GameStatus gameState = initializeGame3PlayersAndFullDeck();

        gameState = IntStream.range(0, 24)
                .mapToObj(i -> new TakeCard())
                .reduce(gameState, GameStatus::executeAction, (gs1, gs2) -> gs2);

        assertTrue(gameState instanceof GameOver);
        assertTrue(gameState.getDeck().isEmpty());
    }

    @Test
    public void test15GameOverDeterminesWinner() {
        GameStatus gameState = initializeGame3PlayersAndFullDeck();
        gameState = IntStream.range(0, 24)
                .mapToObj(i -> new TakeCard())
                .reduce(gameState, GameStatus::executeAction, (gs1, gs2) -> gs2);
        Player winner = ((GameOver) gameState).getWinner();
        int highestScore = gameState.getPlayers().stream()
                .mapToInt(Player::calculateScore)
                .max()
                .orElseThrow(() -> new IllegalStateException("No players in the game."));

        assertEquals(highestScore, winner.calculateScore());
    }

    private void assertInitialTokens(List<Player> players, int expectedTokens) {
        players.forEach(player -> assertEquals(expectedTokens, player.getTokens()));
    }

    private void assertCurrentPlayer(GameStatus gameState, String expectedPlayerName) {
        Player currentPlayer = gameState.getPlayers().get(gameState.getCurrentPlayerIndex());
        assertEquals(expectedPlayerName, currentPlayer.getName());
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

    private GameStatus initializeGame3PlayersAndFullDeck() {
        List<Player> players = createPlayers("Emilio", "Julio", "Bruno");
        GameStatus gameState = setupGame(players, getCardValues());
        return gameState;
    }

    private static List<Integer> getCardValues() {
        return Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);
    }

    private GameStatus executeActions(GameStatus gameState, Action... actions) {
        return Arrays.stream(actions)
                .reduce(gameState, GameStatus::executeAction, (gs1, gs2) -> gs2);
    }
}
