
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.stream.Collectors;

public class Player {
    private final String name;
    private int tokens;
    private final List<Card> cards;

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

    public List<Card> getCards() {
        return new ArrayList<>(cards);
    }

    public void addCard(Card card) {
        cards.add(card);
    }

    public void removeTokens(int amount) {
        if (tokens < amount) {
            throw new IllegalStateException("Insufficient tokens.");
        }
        this.tokens -= amount;
    }

    public void addTokens(int amount) {
        this.tokens += amount;
    }


    public void removeCard(Card card) {
        cards.remove(card);
    }

    public int calculateScore() {
        List<Integer> cardValues = cards.stream()
                .map(Card::getValue)
                .collect(Collectors.toList());
        Collections.sort(cardValues);

        int score = tokens;
        int seriesStart = -1;

        for (int i = 0; i < cardValues.size(); i++) {
            if (seriesStart == -1) {
                seriesStart = cardValues.get(i);
            }

            if (i == cardValues.size() - 1 || cardValues.get(i) + 1 != cardValues.get(i + 1)) {
                score -= seriesStart;
                seriesStart = -1;
            }
        }

        return score;
    }

}
