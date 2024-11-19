import java.util.List;

public class GameStatus {
    private final List<Player> players;
    private int currentPlayerIndex;
    private boolean open;

    public GameStatus(List<Player> players) {
        this.players = players;
        this.currentPlayerIndex = 0;
        this.open = true;
    }

    public Player getCurrentPlayer() {
        return players.get(currentPlayerIndex);
    }

    public boolean isOpen() {
        return open;
    }

    public void closeGame() {
        open = false;
    }

    public void advanceTurn() {
        currentPlayerIndex = (currentPlayerIndex + 1) % players.size();
    }
}
