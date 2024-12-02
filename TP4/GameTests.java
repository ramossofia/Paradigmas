import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;

public class GameTests {

    private static final List<Integer> CARD_VALUES = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26);

    private GameStatus setupGame(List<Player> players, List<Integer> cardValues) {
        List<Card> cards = createCards(cardValues);
        return new GameInProgress(players, new Deck(cards));
    }

    private static void assertThrowsLike(Class<? extends Throwable> expectedType, String expectedMessage, Executable executable) {
        Throwable exception = assertThrows(expectedType, executable);
        assertEquals(expectedMessage, exception.getMessage());
    }

    @Test
    public void test01PlayerCountIsBetweenThreeAndSeven() {
        List<Player> players = createPlayers("Emilio", "Julio");

        assertThrowsLike(IllegalArgumentException.class, "The number of players should be between 3 and 7.", () -> setupGame(players, CARD_VALUES));
    }

    @Test
    public void test02DeckInitialization() {
        List<Player> players = createPlayers("Emilio", "Julio", "Bruno");
        List<Integer> invalidCardValues = Arrays.asList(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23);

        assertThrowsLike(IllegalArgumentException.class, "The deck must contain exactly 24 cards.", () -> setupGame(players, invalidCardValues));
    }

    @Test
    public void test03InitialTokensAndCardsFor3Players() {
        GameStatus gameState = setupGameWithPlayers(3);
        List<Player> players = gameState.getPlayers();

        assertInitialTokens(players, GameInProgress.calculateInitialTokens(players.size()));
    }

    @Test
    public void test04InitialTokensAndCardsFor6Players() {
        GameStatus gameState = setupGameWithPlayers(6);
        List<Player> players = gameState.getPlayers();

        assertInitialTokens(players, GameInProgress.calculateInitialTokens(players.size()));
    }

    @Test
    public void test05InitialTokensAndCardsFor7Players() {
        GameStatus gameState = setupGameWithPlayers(7);
        List<Player> players = gameState.getPlayers();

        assertInitialTokens(players, GameInProgress.calculateInitialTokens(players.size()));
    }

    @Test
    public void test06TurnRotationIsCorrect() {
        GameStatus gameState = setupGameWithPlayers(3);

        gameState = gameState.executeAction(new PlaceToken());
        assertCurrentPlayer(gameState, "Julio");
    }

    @Test
    public void test07TurnWrapsAroundToFirstPlayer() {
        GameStatus gameState = setupGameWithPlayers(3);

        gameState = executeActions(gameState, new TakeCard(), new PlaceToken(), new PlaceToken(), new PlaceToken());
        assertCurrentPlayer(gameState, "Emilio");
    }
    
    @Test
    public void test09PlaceTokenAndPassTurn() {
        GameStatus gameState = setupGameWithPlayers(3);

        gameState = executeActions(gameState, new TakeCard(), new PlaceToken());
        assertCurrentPlayer(gameState, "Julio");
    }

    @Test
    public void test10TakeLastCardFromDeck() {
        GameStatus gameState = setupGameWithPlayers(3);

        gameState = gameState.executeAction(new TakeCard());

        assertEquals(3, gameState.getCurrentPlayer().getCards().iterator().next().getValue());
    }

    @Test
    public void test11PlayerPlacesTokenAndNextPlayerTakesCard() {
        GameStatus gameState = setupGameWithPlayers(3);

        gameState = executeActions(gameState, new TakeCard(), new PlaceCard(), new PlaceToken(), new TakeCard());

        assertEquals(12, gameState.getPlayers().get(1).getTokens());
    }

    @Test
    public void test12PlayerStartsWithoutTokensMustTakeCard() {
        GameStatus gameState = setupGameWithPlayers(3);

        gameState.getCurrentPlayer().setTokens(0);

        gameState = gameState.executeAction(new PlaceToken());

        assertEquals(1, gameState.getCurrentPlayer().getCards().size());
    }

    @Test
    public void test13CalculatePointsWithSeries() {
        GameStatus gameState = setupGameWithPlayers(3);

        gameState = executeActions(gameState, new TakeCard(), new TakeCard(), new TakeCard());

        Player currentPlayer = gameState.getCurrentPlayer();
        assertEquals(-3 + 11, currentPlayer.calculateScore());
    }

    @Test
    public void test14GameEndsWhenDeckIsEmpty() {
        GameStatus gameState = setupGameWithPlayers(3);

        gameState = IntStream.range(0, 24)
                .mapToObj(i -> new TakeCard())
                .reduce(gameState, GameStatus::executeAction, (gs1, gs2) -> gs2);

        assertTrue(gameState instanceof GameOver);
        assertTrue(gameState.getDeck().isEmpty());
    }

    @Test
    public void test15GameOverDeterminesWinner() {
        GameStatus gameState = setupGameWithPlayers(3);
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

    private GameStatus setupGameWithPlayers(int numberOfPlayers) {
        List<Player> players = createPlayers("Emilio", "Julio", "Bruno", "Mariela", "Marcelo", "Kun", "Diego").subList(0, numberOfPlayers);
        return setupGame(players, CARD_VALUES);
    }

    private void assertInitialTokens(List<Player> players, int expectedTokens) {
        players.forEach(player -> assertEquals(expectedTokens, player.getTokens()));
    }

    private void assertCurrentPlayer(GameStatus gameState, String expectedPlayerName) {
        assertEquals(expectedPlayerName, gameState.getCurrentPlayer().getName());
    }

    private List<Player> createPlayers(String... playerNames) {
        return Arrays.stream(playerNames)
                .map(Player::new)
                .collect(Collectors.toList());
    }

    private List<Card> createCards(List<Integer> cardValues) {
        return cardValues.stream()
                .map(value -> new Card(value))
                .collect(Collectors.toList());
    }

    private GameStatus executeActions(GameStatus gameState, Action... actions) {
        return Arrays.stream(actions)
                .reduce(gameState, GameStatus::executeAction, (gs1, gs2) -> gs2);
    }
}
