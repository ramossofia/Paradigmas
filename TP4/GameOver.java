import java.util.List;

public class GameOver extends GameStatus {

    /**
     * Constructs a GameOver instance with the final state of players and the deck.
     *
     * @param players The list of players in the game.
     * @param deck    The final deck state.
     */
    public GameOver(List<Player> players, Deck deck) {
        super(List.copyOf(players), deck, -1); // -1 indicates no current player
    }

    /**
     * The game is over, so moving to the next player has no effect.
     *
     * @return The current GameOver instance.
     */
    @Override
    public GameStatus nextPlayer() {
        return this;
    }

    /**
     * The game is over, so executing actions has no effect.
     *
     * @param action The action to execute (ignored).
     * @return The current GameOver instance.
     */
    @Override
    public GameStatus executeAction(Action action) {
        return this;
    }

    /**
     * The game is already over, so no further checks are necessary.
     *
     * @return The current GameOver instance.
     */
    @Override
    public GameStatus checkGameOver() {
        return this;
    }

    @Override
    public String toString() {
        return "GameOver{" +
                "players=" + getPlayers() +
                ", deck=" + getDeck() +
                '}';
    }
}
