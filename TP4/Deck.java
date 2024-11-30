// src/Deck.java
import java.util.*;

public class Deck {
    private List<Integer> cards;
    private int tokensOnTopCard;

    public Deck() {
        this.cards = new ArrayList<>();
        this.tokensOnTopCard = 0;
    }

    public void initializeDeck(List<Integer> cardValues) {
        this.cards = new ArrayList<>(cardValues);
        Collections.shuffle(cards);
    }

    public int size() {
        return cards.size();
    }

    public boolean isEmpty() {
        return cards.isEmpty();
    }

    public int getTopCard() {
        return cards.get(cards.size() - 1);
    }

    public void removeTopCard() {
        if (!cards.isEmpty()) {
            cards.remove(cards.size() - 1);
        } else {
            throw new IllegalStateException("No cards left in the deck.");
        }
    }

    public int getTokensOnTopCard() {
        return tokensOnTopCard;
    }

    public void addTokenToTopCard() {
        tokensOnTopCard++;
    }

    public void resetTokensOnTopCard() {
        tokensOnTopCard = 0;
    }

    public int drawCard() {
        if (!cards.isEmpty()) {
            return cards.remove(cards.size() - 1);
        } else {
            throw new IllegalStateException("No cards left in the deck.");
        }
    }
}
