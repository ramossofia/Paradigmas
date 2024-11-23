
// src/Game.java
import java.util.List;

public class Game {
    private List<Player> players;
    private Deck deck;
    private Card currentCard;
    private int currentPlayerIndex;
    private boolean gameInProgress;

    public void startGame(List<Player> players, int fichasIniciales, List<Integer> cards) {
        this.players = players;
        this.deck = new Deck(cards);
        this.currentPlayerIndex = 0;
        this.gameInProgress = true;

        // Distribute tokens to each player
        for (Player player : players) {
            player.setTokens(fichasIniciales);
        }
    }

    public Player getCurrentPlayer() {
        return players.get(currentPlayerIndex);
    }

    public void executeAction(Action action) {
        if (!gameInProgress) {
            throw new IllegalStateException("The game has ended.");
        }

        Player currentPlayer = getCurrentPlayer();
        if (action.getPlayer() != currentPlayer) {
            throw new IllegalStateException("It's not " + action.getPlayer().getName() + "'s turn.");
        }

        if (currentPlayer.hasPlayedThisTurn()) {
            throw new IllegalStateException(currentPlayer.getName() + " has already played this turn.");
        }

        // Execute the action
        action.execute(currentPlayer, this);

        // Mark the current player as having played this turn
        currentPlayer.setHasPlayedThisTurn(true);

        if (deck.isEmpty()) {
            endGame();
        } else {
            nextTurn(); // Move to the next player
        }
    }

    private void nextTurn() {
        // Move to the next player
        currentPlayerIndex = (currentPlayerIndex + 1) % players.size();
        // Reset the player's turn state for the next player
        getCurrentPlayer().resetTurn();
    }

    private void endGame() {
        gameInProgress = false;
    }
}
