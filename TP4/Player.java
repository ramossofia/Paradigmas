// src/Player.java
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

public class Player {
    private String name;
    private int tokens;
    private List<Integer> cards;

    public Player(String name) {
        this.name = name;
        this.tokens = 0;
        this.cards = new ArrayList<>();
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

    public void addTokens(int tokens) {
        this.tokens += tokens;
    }

    public List<Integer> getCards() {
        return cards;
    }

    public Player addCard(int card) {
        this.cards.add(card);
        return this;
    }

    public int calculateScore() {
        // Ordenar las cartas del jugador
        List<Integer> sortedCards = new ArrayList<>(cards);
        Collections.sort(sortedCards);

        int score = tokens; // Cada token es un punto positivo
        int seriesStart = -1;

        for (int i = 0; i < sortedCards.size(); i++) {
            if (seriesStart == -1) {
                seriesStart = sortedCards.get(i);
            }

            // Si la carta actual no es consecutiva con la anterior, o es la última carta
            if (i == sortedCards.size() - 1 || sortedCards.get(i) + 1 != sortedCards.get(i + 1)) {
                score -= seriesStart; // Restar el valor de la carta más pequeña de la serie
                seriesStart = -1; // Reiniciar la serie
            }
        }

        return score;
    }
}
