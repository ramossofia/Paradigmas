import java.util.List;
import java.util.Optional;


public class Deck {
    private List<Card> cards;

    public Deck(List<Card> cards) {
        this.cards = cards;
    }

    public Optional<Card> drawCard() {
        return cards.isEmpty() ? Optional.empty() : Optional.of(cards.get(0));
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
    public void removeCard(Card card) {
        cards.remove(card);
    }


}
