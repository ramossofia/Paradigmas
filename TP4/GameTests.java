import org.junit.jupiter.api.Test;

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

    @Test
    public void test01PlayerCountIsBetweenThreeAndSeven() {
        List<Player> players = createPlayers("Emilio", "Julio");

        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> setupGame(players, CARD_VALUES));

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
    public void test08PlayerCannotPlayTwiceInARow() {
        GameStatus initialGameState = setupGameWithPlayers(3);

        GameStatus gameState = executeActions(initialGameState, new TakeCard(), new PlaceToken(), new PlaceToken());

        assertThrows(IllegalStateException.class, () -> {
            if (!gameState.getPlayers().get(gameState.getCurrentPlayerIndex()).getName().equals("Emilio")) {
                throw new IllegalStateException("It's not Emilio's turn.");
            }
        });
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

        assertEquals(3, gameState.getPlayers().get(gameState.getCurrentPlayerIndex()).getCards().get(0).getValue(), "La carta tomada debe ser la 26.");
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

        gameState.getPlayers().get(0).setTokens(0);

        gameState = gameState.executeAction(new PlaceToken());

        assertEquals(1, gameState.getPlayers().get(0).getCards().size());
    }

    @Test
    public void test13CalculatePointsWithSeries() {
        GameStatus gameState = setupGameWithPlayers(3);

        gameState = executeActions(gameState, new TakeCard(), new TakeCard(), new TakeCard());

        assertEquals(-3 + 11, gameState.getPlayers().get(0).calculateScore());
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
        assertEquals(expectedPlayerName, gameState.getPlayers().get(gameState.getCurrentPlayerIndex()).getName());
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

    private GameStatus executeActions(GameStatus gameState, Action... actions) {
        return Arrays.stream(actions)
                .reduce(gameState, GameStatus::executeAction, (gs1, gs2) -> gs2);
    }
}
