import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Player {
    private String name;
    private int tokens;
    private boolean hasPlayedThisTurn = false; // Flag to track if the player has played this turn
    private List<Card> cards = new ArrayList<>(); // Define the cards variable

    public Player(String name, int initialTokens) {
        this.name = name;
        this.tokens = initialTokens;
    }

    public void addCard(Card card) {
        this.cards.add(card);
    }

    public String getName() {
        return name;
    }

    public int getTokens() {
        return tokens;
    }

    public void setTokens(int tokens) {
        this.tokens = tokens;
    }

    public boolean hasPlayedThisTurn() {
        return hasPlayedThisTurn;
    }

    public void resetTurn() {
        hasPlayedThisTurn = false;  // Reset at the start of each turn
    }

    public void decrementTokens() {
        if (tokens <= 0) {
            throw new IllegalStateException(name + " has no tokens left.");
        }
        tokens--;  // Decrease token count
    }

    public void setHasPlayedThisTurn(boolean played) {
        this.hasPlayedThisTurn = played;
    }

    public void takeCard(Card card, int tokens) {
        this.tokens += tokens;
    }

    public int calculatePoints() {
        int points = tokens; // Each token is a positive point
        List<Integer> cardValues = new ArrayList<>();
        for (Card card : cards) {
            cardValues.add(card.getValue());
        }
        Collections.sort(cardValues);

        int seriesStart = -1;
        for (int i = 0; i < cardValues.size(); i++) {
            if (seriesStart == -1) {
                seriesStart = cardValues.get(i);
            }
            if (i == cardValues.size() - 1 || cardValues.get(i) + 1 != cardValues.get(i + 1)) {
                points -= seriesStart;
                seriesStart = -1;
            }
        }
        return points;
    }
    public List<Card> getCards() {
        return cards;
    }
}
