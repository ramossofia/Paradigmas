import java.util.ArrayList;
import java.util.List;
import java.util.Map;


public class GameInProgress extends GameStatus {
    private List<Card> tableCards;

    public GameInProgress(List<Player> players, Deck deck) {
        super(initializePlayers(players), deck, 0);
        if (players.size() < 3 || players.size() > 7) {
            throw new IllegalArgumentException("The number of players should be between 3 and 7.");
        }
        if (deck.size() != 24) {
            throw new IllegalArgumentException("The deck must contain exactly 24 cards.");
        }
        this.tableCards = new ArrayList<>();
    }


    private static List<Player> initializePlayers(List<Player> players) {
        int initialTokens = calculateInitialTokens(players.size());
        players.forEach(player -> player.setTokens(initialTokens));
        return players;
    }

    public static int calculateInitialTokens(int numberOfPlayers) {
        Map<Integer, Integer> tokensMap = Map.of(
            6, 9,
            7, 7
        );
        return tokensMap.getOrDefault(numberOfPlayers, 11);
    }

    public void setCurrentPlayerIndex(int currentPlayerIndex) {
    int numberOfPlayers = getPlayers().size();
    super.currentPlayerIndex = (currentPlayerIndex % numberOfPlayers + numberOfPlayers) % numberOfPlayers;
}

    @Override
    public void nextPlayer() {
        int newCurrentPlayerIndex = (getCurrentPlayerIndex() + 1) % getPlayers().size();
        setCurrentPlayerIndex(newCurrentPlayerIndex);
        checkGameOver();
    }

    @Override
    public GameStatus executeAction(Action action) {
        GameStatus newState = action.execute(this);
        return newState.checkGameOver();
    }

    @Override
    public GameStatus checkGameOver() {
        if (getDeck().isEmpty() && tableCards.isEmpty()) {
            return new GameOver(getPlayers(), getDeck());
        }
        return this;
    }

    @Override
    public String toString() {
        return "GameInProgress{" +
                "players=" + getPlayers() +
                ", deck=" + getDeck() +
                ", currentPlayerIndex=" + getCurrentPlayerIndex() +
                ", tableCards=" + tableCards +
                '}';
    }

    public boolean hasCardsOnTable() {
        return !tableCards.isEmpty();
    }

    public Card takeCardFromTable() {
        if (tableCards.isEmpty()) {
            throw new IllegalStateException("No cards on the table.");
        }
        return tableCards.remove(0);
    }

    public void addCardToTable(Card card) {
        tableCards.add(card);
    }

    public void addTokensToLastTableCard(int tokens) {
        if (tableCards.isEmpty()) {
            throw new IllegalStateException("No cards on the table to add tokens.");
        }
        tableCards.get(tableCards.size() - 1).addTokens(tokens);
    }
}
