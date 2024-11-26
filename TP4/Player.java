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
        int points = -tokens;
        List<Integer> cardValues = new ArrayList<>();

        for (Card card : cards) {
            cardValues.add(card.getValue());
        }

        // Ordenar las cartas
        Collections.sort(cardValues);

        int totalPoints = 0;
        Integer seriesStart = null; // Valor inicial de la serie

        for (int i = 0; i < cardValues.size(); i++) {
            if (seriesStart == null) {
                // Comenzar una nueva serie
                seriesStart = cardValues.get(i);
            }

            // Si no es consecutiva o es la última carta, sumar los puntos de la serie
            if (i == cardValues.size() - 1 || cardValues.get(i) + 1 != cardValues.get(i + 1)) {
                totalPoints += seriesStart; // Sumar el valor más bajo de la serie
                seriesStart = null; // Reiniciar la serie
            }
        }

        return -totalPoints -points; // Combinar puntos negativos de cartas y positivos de fichas
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
