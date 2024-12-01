import java.util.List;

public abstract class GameStatus {
    protected final List<Player> players;
    protected final Deck deck;
    protected int currentPlayerIndex;

    public GameStatus(List<Player> players, Deck deck, int currentPlayerIndex) {
        this.players = List.copyOf(players);
        this.deck = deck;
        this.currentPlayerIndex = currentPlayerIndex;
    }

    public List<Player> getPlayers() {
        return players;
    }

    public Deck getDeck() {
        return deck;
    }

    public int getCurrentPlayerIndex() {
        return currentPlayerIndex;
    }

    public abstract void nextPlayer();

    public abstract GameStatus executeAction(Action action);

    protected GameStatus checkGameOver() {
        if (deck.isEmpty()) {
            return new GameOver(players, deck);
        }
        return this;
    }

    public Player getCurrentPlayer() {
        return players.get(currentPlayerIndex);
    }
}
