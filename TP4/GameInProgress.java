import java.util.ArrayList;
import java.util.List;

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
        switch (numberOfPlayers) {
            case 6:
                return 9;
            case 7:
                return 7;
            default:
                return 11;
        }
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
        return tableCards.remove(0);
    }

    public void addCardToTable(Card card) {
        tableCards.add(card);
    }

    public void addTokensToLastTableCard(int tokens) {
        tableCards.get(tableCards.size() - 1).addTokens(tokens);
    }
}
