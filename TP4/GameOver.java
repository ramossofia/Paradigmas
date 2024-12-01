// src/GameOver.java
import java.util.List;

public class GameOver extends GameStatus {

    public GameOver(List<Player> players, Deck deck) {
        super(List.copyOf(players), deck, -1); // -1 indicates no current player
    }

    @Override
    public void nextPlayer() {
        // No action needed as the game is over
    }

    @Override
    public GameStatus executeAction(Action action) {
        return this;
    }

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
