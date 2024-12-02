import java.util.List;
import java.util.stream.IntStream;

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
        final Turn[] current = {firstTurn};

        IntStream.range(1, players.size())
                .mapToObj(i -> new Turn(players.get(i)))
                .forEach(next -> {
                    current[0].setNextTurn(next);
                    current[0] = next;
                });

        current[0].setNextTurn(firstTurn);
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
        return new GameOver(players, deck);
    }
}
