import java.util.List;

public class GameOver extends GameStatus {
    private Player winner;

    public GameOver(List<Player> players, Deck deck) {
        super(players, deck);
        this.winner = determineWinner(players);
    }

    @Override
    public void nextPlayer() {
        // No-op in GameOver
    }

    @Override
    public GameStatus executeAction(Action action) {
        return this; // No action can modify the game after it ends
    }

    @Override
    public GameStatus checkGameOver() {
        return this; // Already in a "game over" state
    }

    private Player determineWinner(List<Player> players) {
        return players.stream()
                .max((p1, p2) -> Integer.compare(p1.calculateScore(), p2.calculateScore()))
                .orElseThrow(() -> new IllegalStateException("No players in the game."));
    }

    public Player getWinner() {
        return winner;
    }
}
