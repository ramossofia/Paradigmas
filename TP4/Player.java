import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Player {
    private String name;
    private int tokens;
    private boolean hasPlayedThisTurn = false;
    private List<Card> cards = new ArrayList<>();

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
        hasPlayedThisTurn = false;
    }


    public void setHasPlayedThisTurn(boolean played) {
        this.hasPlayedThisTurn = played;
    }

    public int calculatePoints() {
        int points = tokens;  // Start with the player's tokens
        List<Integer> cardValues = new ArrayList<>();

        // Add the values of the cards to the list
        for (Card card : cards) {
            cardValues.add(card.getValue());
        }

        // Sort the cards
        Collections.sort(cardValues);

        int seriesStart = -1;  // The first card in a potential series
        boolean inSeries = false;  // Flag to check if we're in a series

        for (int i = 0; i < cardValues.size(); i++) {
            // Start a new series
            if (seriesStart == -1) {
                seriesStart = cardValues.get(i);
                inSeries = true;
            }

            // Check if the current card is part of a consecutive sequence
            if (i == cardValues.size() - 1 || cardValues.get(i) + 1 != cardValues.get(i + 1)) {
                if (inSeries) {
                    points -= seriesStart;  // Subtract the smallest card value in the series
                } else {
                    points -= cardValues.get(i);  // No series, subtract the card's value
                }
                seriesStart = -1;  // Reset the series start
                inSeries = false;  // End the series
            }
        }

        return -points;  // Points are negative in this game
    }




    public List<Card> getCards() {
        return cards;
    }

    public void takeCard(Card card, int tokens) {
        this.tokens += tokens; // Sumar los tokens de la carta
        this.cards.add(card);  // Agregar la carta a la lista de cartas del jugador
    }


    public void decrementTokens() {
        if (tokens <= 0) {
            throw new IllegalStateException(name + " has no tokens left.");
        }
        tokens--;
    }

}
