import java.util.List;

public abstract class GameStatus {
    protected final List<Player> players;
    protected final Deck deck;
    protected Turn currentTurn;

    public GameStatus(List<Player> players, Deck deck) {
        this.players = List.copyOf(players);
        this.deck = deck;
        initializeTurns(players);
    }

    private void initializeTurns(List<Player> players) {
        Turn firstTurn = new Turn(players.get(0));
        Turn current = firstTurn;

        for (int i = 1; i < players.size(); i++) {
            Turn next = new Turn(players.get(i));
            current.setNextTurn(next);
            current = next;
        }
        // Circular turn logic
        current.setNextTurn(firstTurn);
        this.currentTurn = firstTurn;
    }

    public List<Player> getPlayers() {
        return players;
    }

    public Deck getDeck() {
        return deck;
    }

    public Player getCurrentPlayer() {
        return currentTurn.getPlayer();
    }

    public void nextPlayer() {
        currentTurn = currentTurn.getNextTurn();
    }

    public abstract GameStatus executeAction(Action action);

    protected GameStatus checkGameOver() {
        if (deck.isEmpty()) {
            return new GameOver(players, deck);
        }
        return this;
    }
}
