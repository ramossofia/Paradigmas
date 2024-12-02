import java.util.LinkedList;
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
        LinkedList<Turn> turns = new LinkedList<>();
        players.forEach(player -> turns.add(new Turn(player)));

        IntStream.range(0, turns.size())
                .forEach(i -> {
                    Turn current = turns.get(i);
                    Turn next = turns.get((i + 1) % turns.size());
                    current.setNextTurn(next);
                });

        this.currentTurn = turns.getFirst();
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
