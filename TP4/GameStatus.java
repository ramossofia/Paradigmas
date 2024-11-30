// src/GameStatus.java
import java.util.List;

public abstract class GameStatus {
    protected List<Player> players;
    protected Deck deck;
    protected int currentPlayerIndex;

    public GameStatus(List<Player> players, Deck deck) {
        this.players = players;
        this.deck = deck;
        this.currentPlayerIndex = 0;
    }

    public abstract GameStatus nextPlayer();
    public abstract GameStatus executeAction(Action action);

    public List<Player> getPlayers() {
        return players;
    }

    public Deck getDeck() {
        return deck;
    }

    public int getCurrentPlayerIndex() {
        return currentPlayerIndex;
    }

    protected GameStatus checkGameOver() {
        if (deck.isEmpty()) {
            return new GameOver(players, deck);
        }
        return this;
    }
}
