import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

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
        int points = -tokens;
        List<Integer> cardValues = cards.stream()
                .map(Card::getValue)
                .sorted()
                .collect(Collectors.toList());

        int totalPoints = 0;
        Integer seriesStart = null;

        for (int i = 0; i < cardValues.size(); i++) {
            if (seriesStart == null) {
                seriesStart = cardValues.get(i);
            }


            if (i == cardValues.size() - 1 || cardValues.get(i) + 1 != cardValues.get(i + 1)) {
                totalPoints += seriesStart; // Add the series start to the total points
                seriesStart = null;
            }
        }

        return -totalPoints - points;
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
