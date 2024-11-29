import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Player {
    private String name;
    private int tokens;
    private List<Card> cards;
    private boolean placedToken;

    public Player(String name) {
        this.name = name;
        this.cards = new ArrayList<>();
        this.placedToken = false;
    }

    public void setTokens(int tokens) {
        this.tokens = tokens;
    }

    public int tokens() {
        return tokens;
    }

    public void addCard(Card card) {
        cards.add(card);
        tokens += card.getTokens();
    }

    public List<Card> cards() {
        return cards;
    }

    public int calculatePoints() {
        if (cards.isEmpty()) {
            return 0;
        }

        Collections.sort(cards, (c1, c2) -> Integer.compare(c1.getValue(), c2.getValue()));
        int totalPoints = 0;
        int seriesStart = 0;

        while (seriesStart < cards.size()) {
            int seriesEnd = seriesStart;
            while (seriesEnd + 1 < cards.size() && cards.get(seriesEnd + 1).getValue() == cards.get(seriesEnd).getValue() + 1) {
                seriesEnd++;
            }

            if (seriesEnd > seriesStart) {
                totalPoints += cards.get(seriesStart).getValue();
            } else {
                totalPoints += cards.get(seriesStart).getValue();
            }

            seriesStart = seriesEnd + 1;
        }

        return -totalPoints;
    }

    public void addTokens(int tokens) {
        this.tokens += tokens;
    }

    public void placeToken() {
        if (tokens > 0 && !placedToken) {
            tokens--;
            placedToken = true;
        } else if (placedToken) {
            throw new IllegalStateException("Cannot place more than one token per turn.");
        } else {
            throw new IllegalStateException("No tokens available to place.");
        }
    }

    public boolean hasPlacedToken() {
        return placedToken;
    }

    public void resetPlacedToken() {
        placedToken = false;
    }
}
