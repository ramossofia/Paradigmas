import java.util.List;

public class GameOver extends GameStatus {
    private Player winner;

    public GameOver(List<Player> players, Deck deck) {
        super(List.copyOf(players), deck, -1);
        this.winner = determineWinner(players);
    }

    @Override
    public void nextPlayer() {
    }

    @Override
    public GameStatus executeAction(Action action) {
        return this;
    }

    @Override
    public GameStatus checkGameOver() {
        return this;
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
