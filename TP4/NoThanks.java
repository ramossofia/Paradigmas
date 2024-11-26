// File: NoThanks.java
import java.util.List;
import java.util.stream.Collectors;

public class NoThanks {
    private List<Player> players;
    private List<Card> deck;
    private int currentPlayerIndex;

    public void startGame(List<Player> players, int initialTokens, List<Integer> cardValues) {
        if (players.size() < 3 || players.size() > 7) {
            throw new IllegalArgumentException("The number of players must be between 3 and 7.");
        }

        for (int value : cardValues) {
            if (value < 3 || value > 35) {
                throw new IllegalArgumentException("Card values must be between 3 and 35.");
            }
        }

        this.players = players;
        this.deck = cardValues.stream().map(value -> new Card(value, 0)).collect(Collectors.toList());
        this.currentPlayerIndex = 0;

        for (Player player : players) {
            player.setTokens(initialTokens);
        }
    }

    public List<Card> getDeck() {
        return deck;
    }

    public List<Player> getPlayers() {
        return players;
    }

    public int getCurrentPlayerIndex() {
        return currentPlayerIndex;
    }

    public void advanceToNextPlayer() {
        currentPlayerIndex = (currentPlayerIndex + 1) % players.size();
    }

    public void takeCard(Player player) {
        if (!deck.isEmpty()) {
            Card card = deck.remove(0);
            player.takeCard(card, card.getTokens());
        }
    }

    public void executeAction(Action action) {
        Player currentPlayer = players.get(currentPlayerIndex);

        if (!currentPlayer.equals(action.getPlayer())) {
            throw new IllegalStateException("It's not " + action.getPlayer().getName() + "'s turn.");
        }

        if (action instanceof TakeCard) {
            takeCard(currentPlayer);
        } else if (action instanceof PlaceToken) {
            currentPlayer.decrementTokens();
            deck.get(0).addToken();
            advanceToNextPlayer();
        }
    }

    public void checkAndForceTakeCard() {
        Player currentPlayer = players.get(currentPlayerIndex);
        if (currentPlayer.getTokens() == 0) {
            takeCard(currentPlayer);
        }
    }

    public void endGame() {
        System.out.println("Game over!");
        for (Player player : players) {
            System.out.println(player.getName() + " has " + player.calculatePoints() + " points.");
        }
    }
}
