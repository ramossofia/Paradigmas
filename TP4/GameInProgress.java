import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class GameInProgress extends GameStatus {
    private List<Card> tableCards;

    public GameInProgress(List<Player> players, Deck deck) {
        super(players, deck);
        if (players.size() < 3 || players.size() > 7) {
            throw new IllegalArgumentException("The number of players should be between 3 and 7.");
        }
        if (deck.size() != 24) {
            throw new IllegalArgumentException("The deck must contain exactly 24 cards.");
        }
        this.tableCards = new ArrayList<>();
        initializePlayers(players);
    }

    private void initializePlayers(List<Player> players) {
        int initialTokens = calculateInitialTokens(players.size());
        players.forEach(player -> player.setTokens(initialTokens));
    }

    public static int calculateInitialTokens(int numberOfPlayers) {
        Map<Integer, Integer> tokensMap = Map.of(
                6, 9,
                7, 7
        );
        return tokensMap.getOrDefault(numberOfPlayers, 11);
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
