import java.util.List;

/**
 * Represents the abstract status of a game, providing the base structure for different game states.
 */
public abstract class GameStatus {

    // Immutable list of players in the game.
    protected final List<Player> players;

    // Immutable deck of cards representing the current game state.
    protected final Deck deck;

    // Index of the current player (0-based). -1 indicates no active player.
    protected final int currentPlayerIndex;

    /**
     * Constructs a GameStatus with the default current player index (0).
     *
     * @param players The list of players in the game.
     * @param deck    The deck of cards.
     */
    public GameStatus(List<Player> players, Deck deck) {
        this(players, deck, 0);
    }

    /**
     * Constructs a GameStatus with the specified current player index.
     *
     * @param players            The list of players in the game.
     * @param deck               The deck of cards.
     * @param currentPlayerIndex The index of the current player.
     */
    public GameStatus(List<Player> players, Deck deck, int currentPlayerIndex) {
        this.players = List.copyOf(players); // Ensures immutability of the player list
        this.deck = deck; // Assuming Deck is immutable
        this.currentPlayerIndex = currentPlayerIndex;
    }

    /**
     * Gets the immutable list of players.
     *
     * @return The list of players.
     */
    public List<Player> getPlayers() {
        return players;
    }

    /**
     * Gets the current state of the deck.
     *
     * @return The deck.
     */
    public Deck getDeck() {
        return deck;
    }

    /**
     * Gets the index of the current player.
     *
     * @return The current player index.
     */
    public int getCurrentPlayerIndex() {
        return currentPlayerIndex;
    }

    /**
     * Advances to the next player in the game.
     *
     * @return A new GameStatus with the updated current player.
     */
    public abstract GameStatus nextPlayer();

    /**
     * Executes a game action, modifying the game state accordingly.
     *
     * @param action The action to execute.
     * @return A new GameStatus reflecting the result of the action.
     */
    public abstract GameStatus executeAction(Action action);

    /**
     * Checks if the game is over and transitions to the appropriate state.
     *
     * @return A new GameStatus representing the next state of the game.
     */
    protected GameStatus checkGameOver() {
        if (deck.isEmpty()) {
            return new GameOver(players, deck);
        }
        return this;
    }
}
