// src/GameInProgress.java
import java.util.*;

public class GameInProgress extends GameStatus {

    public GameInProgress(List<Player> players, Deck deck) {
        super(players, deck);
        int initialTokens = calculateInitialTokens(players.size());
        for (Player player : players) {
            player.setTokens(initialTokens);
        }
    }

    public int calculateInitialTokens(int numberOfPlayers) {
        if (numberOfPlayers <= 5) {
            return 11;
        } else if (numberOfPlayers == 6) {
            return 9;
        } else {
            return 7;
        }
    }

    @Override
    public GameStatus nextPlayer() {
        currentPlayerIndex = (currentPlayerIndex + 1) % players.size();
        return checkGameOver();
    }

    @Override
    public GameStatus executeAction(Action action) {
        Player currentPlayer = players.get(currentPlayerIndex);
        if (currentPlayer.getTokens() == 0 && action instanceof PlaceToken) {
            return this; // No puede colocar un token si no tiene tokens
        }
        GameStatus newState = action.execute(this, currentPlayer);
        return newState.checkGameOver();
    }

    public Deck getDeck() {
        return this.deck;
    }
}
