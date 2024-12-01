
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class Deck {
    private List<Card> cards;

    public Deck(List<Integer> cardValues) {
        this.cards = cardValues.stream()
                .map(value -> new Card(value, 0))
                .collect(Collectors.toUnmodifiableList());
    }

    private Deck(List<Card> cards, boolean isPrebuilt) {
        this.cards = List.copyOf(cards);
    }

    public Optional<Card> drawCard() {
        return cards.isEmpty() ? Optional.empty() : Optional.of(cards.get(0));
    }

    public Deck removeTopCard() {
        if (cards.isEmpty()) {
            throw new IllegalStateException("The deck is empty.");
        }
        return new Deck(cards.subList(1, cards.size()), true);
    }

    public void addTokensToTopCard(int tokens) {
        if (cards.isEmpty()) {
            throw new IllegalStateException("The deck is empty.");
        }
        cards.get(0).addTokens(tokens);
    }

    public boolean isEmpty() {
        return cards.isEmpty();
    }

    public int size() {
        return cards.size();
    }

    @Override
    public String toString() {
        return "Deck{" +
                "cards=" + cards +
                '}';
    }
}
