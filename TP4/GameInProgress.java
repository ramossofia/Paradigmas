import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class GameInProgress extends GameStatus {

    /**
     * Constructs a new game in progress with the specified players and deck, starting with the first player.
     *
     * @param players List of players in the game.
     * @param deck    The deck of cards for the game.
     */
    public GameInProgress(List<Player> players, Deck deck) {
        super(List.copyOf(players), deck, 0); // Ensure players are immutable
    }

    /**
     * Constructs a new game in progress with the specified players, deck, and current player index.
     *
     * @param players            List of players in the game.
     * @param deck               The deck of cards for the game.
     * @param currentPlayerIndex The index of the current player.
     */
    public GameInProgress(List<Player> players, Deck deck, int currentPlayerIndex) {
        super(List.copyOf(players), deck, currentPlayerIndex); // Ensure players are immutable
    }

    @Override
    public GameStatus nextPlayer() {
        int newCurrentPlayerIndex = (getCurrentPlayerIndex() + 1) % getPlayers().size();
        return new GameInProgress(getPlayers(), getDeck(), newCurrentPlayerIndex).checkGameOver();
    }

    @Override
    public GameStatus executeAction(Action action) {
        Player currentPlayer = getPlayers().get(getCurrentPlayerIndex());
        GameStatus newState = action.execute(this, currentPlayer);
        return newState.checkGameOver();
    }

    /**
     * Returns a new instance of the game with an updated player list, replacing the current player with the updated one.
     *
     * @param updatedPlayer The updated player to replace the current one.
     * @return A new GameInProgress instance with the updated player.
     */
    public GameInProgress withUpdatedPlayer(Player updatedPlayer) {
        List<Player> updatedPlayers = IntStream.range(0, getPlayers().size())
                .mapToObj(index -> index == getCurrentPlayerIndex() ? updatedPlayer : getPlayers().get(index))
                .collect(Collectors.toUnmodifiableList());
        return new GameInProgress(updatedPlayers, getDeck(), getCurrentPlayerIndex());
    }

    /**
     * Returns a new instance of the game with an updated deck.
     *
     * @param updatedDeck The updated deck to use in the game.
     * @return A new GameInProgress instance with the updated deck.
     */
    public GameInProgress withUpdatedDeck(Deck updatedDeck) {
        return new GameInProgress(getPlayers(), updatedDeck, getCurrentPlayerIndex());
    }

    @Override
    public GameStatus checkGameOver() {
        if (getDeck().isEmpty()) {
            return new GameOver(getPlayers(), getDeck());
        }
        return this;
    }

    /**
     * Calculates the initial number of tokens based on the number of players.
     *
     * @param playerCount The number of players in the game.
     * @return The initial number of tokens for each player.
     * @throws IllegalArgumentException If the player count is outside the valid range.
     */
    public int calculateInitialTokens(int playerCount) {
        return switch (playerCount) {
            case 3, 4, 5 -> 11;
            case 6 -> 9;
            case 7 -> 7;
            default -> throw new IllegalArgumentException("Invalid number of players: " + playerCount);
        };
    }

    @Override
    public String toString() {
        return "GameInProgress{" +
                "players=" + getPlayers() +
                ", deck=" + getDeck() +
                ", currentPlayerIndex=" + getCurrentPlayerIndex() +
                '}';
    }
}
